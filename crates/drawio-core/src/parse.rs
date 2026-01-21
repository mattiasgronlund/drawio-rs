// crates/drawio-core/src/parse.rs
//
// quick-xml 0.38.4 compatible parser for "direct XML" .drawio files.
// - Parses <mxfile> with one or more <diagram>
// - Supports <diagram> containing <mxGraphModel> directly
// - Captures non-whitespace <diagram> text as encoded_payload
// - Attempts to decode encoded diagram payloads into mxGraphModel
// - Preserves unknown attributes into `extra` maps
//
// Trimming policy for .drawio:
// - DO NOT globally trim all text events (Config::trim_text(true) is unsafe)
// - DO ignore whitespace-only text/CDATA nodes (indentation/pretty-printing)
// - DO preserve non-whitespace text exactly where it is meaningful (encoded <diagram> payload)

use crate::model::*;
use base64::Engine;
use base64::engine::general_purpose::STANDARD;
use flate2::read::DeflateDecoder;
use percent_encoding::percent_decode_str;
use quick_xml::Reader;
use quick_xml::events::{BytesEnd, BytesStart, Event};
use std::collections::BTreeMap;
use std::io::{BufRead, Read};
use std::str;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("xml error: {0}")]
    Xml(#[from] quick_xml::Error),

    #[error("attribute error: {0}")]
    Attr(#[from] quick_xml::events::attributes::AttrError),

    #[error("utf8 error: {0}")]
    Utf8(#[from] std::str::Utf8Error),

    #[error("missing required attribute: {0}")]
    MissingAttr(&'static str),

    #[error("invalid number for {field}: {value}")]
    InvalidNumber { field: &'static str, value: String },

    #[error("unexpected structure: {0}")]
    Structure(String),

    #[error("encoding error: {0}")]
    Encoding(#[from] quick_xml::encoding::EncodingError),

    #[error("base64 decode error: {0}")]
    Base64(#[from] base64::DecodeError),

    #[error("deflate decode error: {0}")]
    Deflate(#[from] std::io::Error),
}

pub type ParseResult<T> = Result<T, ParseError>;

/// Parse a .drawio file that contains raw XML:
/// `<mxfile>...<diagram>...<mxGraphModel>...</mxGraphModel>...</diagram></mxfile>`.
///
/// Many real `.drawio` files store the `<diagram>` body as an encoded string.
/// This parser captures such payloads as `Diagram.encoded_payload` if present
/// and attempts to decode them into `Diagram.graph_model`.
pub fn parse_mxfile(xml: &str) -> ParseResult<MxFile> {
    let mut reader = Reader::from_str(xml);
    // IMPORTANT: do not enable global trim_text. We'll ignore whitespace-only text manually.

    let mut buf = Vec::new();

    let mut mxfile: Option<MxFile> = None;
    let mut current_diagram: Option<Diagram> = None;
    let mut current_graph: Option<MxGraphModel> = None;

    let mut current_cell_idx: Option<usize> = None;
    let mut current_geometry_cell_idx: Option<usize> = None;
    let mut in_points_array: bool = false;
    let mut user_object_depth: usize = 0;
    let mut current_user_object: Option<UserObjectContext> = None;
    let mut current_cell_is_user_object: bool = false;
    let mut current_geometry_cell_is_user_object: bool = false;
    let mut order_index: usize = 0;

    loop {
        match reader.read_event_into(&mut buf)? {
            Event::Start(e) => {
                let name = local_name_start(&e)?;
                if is_user_object(&name) {
                    user_object_depth = user_object_depth.saturating_add(1);
                    if user_object_depth == 1 {
                        current_user_object = Some(parse_user_object_ctx(&e)?);
                    }
                    continue;
                }
                match name.as_str() {
                    "mxfile" => {
                        let attrs = attrs_to_map(&e)?;
                        mxfile = Some(MxFile {
                            host: attrs.get("host").cloned(),
                            version: attrs.get("version").cloned(),
                            file_type: attrs.get("type").cloned(),
                            diagrams: Vec::new(),
                        });
                    }
                    "diagram" => {
                        let attrs = attrs_to_map(&e)?;
                        current_diagram = Some(Diagram {
                            id: attrs.get("id").cloned(),
                            name: attrs.get("name").cloned(),
                            encoded_payload: None,
                            graph_model: None,
                        });
                    }
                    "mxGraphModel" => {
                        let attrs = attrs_to_map(&e)?;

                        let mut gm = MxGraphModel {
                            dx: parse_i64_opt(attrs.get("dx"), "dx")?,
                            dy: parse_i64_opt(attrs.get("dy"), "dy")?,
                            grid: parse_bool_opt(attrs.get("grid")),
                            grid_size: parse_i64_opt(attrs.get("gridSize"), "gridSize")?,
                            guides: parse_bool_opt(attrs.get("guides")),
                            tooltips: parse_bool_opt(attrs.get("tooltips")),
                            connect: parse_bool_opt(attrs.get("connect")),
                            arrows: parse_bool_opt(attrs.get("arrows")),
                            fold: parse_bool_opt(attrs.get("fold")),
                            page: parse_bool_opt(attrs.get("page")),
                            page_scale: parse_f64_opt(attrs.get("pageScale"), "pageScale")?,
                            page_width: parse_f64_opt(attrs.get("pageWidth"), "pageWidth")?,
                            page_height: parse_f64_opt(attrs.get("pageHeight"), "pageHeight")?,
                            math: parse_bool_opt(attrs.get("math")),
                            shadow: parse_bool_opt(attrs.get("shadow")),
                            extra: BTreeMap::new(),
                            root: Root { cells: Vec::new() },
                            user_objects: Vec::new(),
                        };

                        // Preserve unknown attributes in extra
                        for (k, v) in attrs {
                            if !is_known_graphmodel_attr(&k) {
                                gm.extra.insert(k, v);
                            }
                        }

                        current_graph = Some(gm);
                    }
                    "mxCell" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxCell outside mxGraphModel".into())
                        })?;
                        if user_object_depth > 0 {
                            let ctx = current_user_object.as_ref();
                            let mut cell = parse_mxcell_with_fallback(
                                &reader,
                                &e,
                                ctx.and_then(|c| c.id.as_deref()),
                                ctx.and_then(|c| c.label.as_deref()),
                            )?;
                            cell.order = order_index;
                            order_index = order_index.saturating_add(1);
                            gm.user_objects.push(cell);
                            current_cell_idx = Some(gm.user_objects.len() - 1);
                            current_cell_is_user_object = true;
                        } else {
                            let mut cell = parse_mxcell(&reader, &e)?;
                            cell.order = order_index;
                            order_index = order_index.saturating_add(1);
                            gm.root.cells.push(cell);
                            current_cell_idx = Some(gm.root.cells.len() - 1);
                            current_cell_is_user_object = false;
                        }
                    }
                    "mxGeometry" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxGeometry outside mxGraphModel".into())
                        })?;
                        let geom = parse_mxgeometry(&reader, &e)?;
                        let idx = current_cell_idx.ok_or_else(|| {
                            ParseError::Structure("mxGeometry found but no current mxCell".into())
                        })?;
                        if current_cell_is_user_object {
                            gm.user_objects[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = true;
                        } else {
                            gm.root.cells[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = false;
                        }
                        current_geometry_cell_idx = Some(idx);
                    }
                    "Array" => {
                        if current_geometry_cell_idx.is_some() && attr_is(&e, "as", "points")? {
                            in_points_array = true;
                        }
                    }
                    "mxPoint" => {
                        let idx = match current_geometry_cell_idx {
                            Some(idx) => idx,
                            None => continue,
                        };
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxPoint outside mxGraphModel".into())
                        })?;
                        let geom = if current_geometry_cell_is_user_object {
                            gm.user_objects[idx].geometry.as_mut()
                        } else {
                            gm.root.cells[idx].geometry.as_mut()
                        }
                        .ok_or_else(|| {
                            ParseError::Structure("mxPoint found but no current mxGeometry".into())
                        })?;
                        let point = parse_mxpoint(&reader, &e)?;
                        assign_geometry_point(geom, point, in_points_array);
                    }
                    _ => {}
                }
            }

            Event::Empty(e) => {
                let name = local_name_start(&e)?;
                if is_user_object(&name) {
                    continue;
                }
                match name.as_str() {
                    "diagram" => {
                        let attrs = attrs_to_map(&e)?;
                        let d = Diagram {
                            id: attrs.get("id").cloned(),
                            name: attrs.get("name").cloned(),
                            encoded_payload: None,
                            graph_model: None,
                        };
                        if let Some(file) = mxfile.as_mut() {
                            file.diagrams.push(d);
                        } else {
                            return Err(ParseError::Structure("diagram outside mxfile".into()));
                        }
                    }
                    "mxCell" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxCell outside mxGraphModel".into())
                        })?;
                        if user_object_depth > 0 {
                            let ctx = current_user_object.as_ref();
                            let cell = parse_mxcell_with_fallback(
                                &reader,
                                &e,
                                ctx.and_then(|c| c.id.as_deref()),
                                ctx.and_then(|c| c.label.as_deref()),
                            )?;
                            gm.user_objects.push(cell);
                        } else {
                            let cell = parse_mxcell(&reader, &e)?;
                            gm.root.cells.push(cell);
                        }
                        // empty cell can't have children like mxGeometry, so don't set current_cell_idx
                    }
                    "mxGeometry" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxGeometry outside mxGraphModel".into())
                        })?;
                        let geom = parse_mxgeometry(&reader, &e)?;
                        let idx = current_cell_idx.ok_or_else(|| {
                            ParseError::Structure("mxGeometry found but no current mxCell".into())
                        })?;
                        if current_cell_is_user_object {
                            gm.user_objects[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = true;
                        } else {
                            gm.root.cells[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = false;
                        }
                        current_geometry_cell_idx = Some(idx);
                    }
                    "mxPoint" => {
                        let idx = match current_geometry_cell_idx {
                            Some(idx) => idx,
                            None => continue,
                        };
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxPoint outside mxGraphModel".into())
                        })?;
                        let geom = if current_geometry_cell_is_user_object {
                            gm.user_objects[idx].geometry.as_mut()
                        } else {
                            gm.root.cells[idx].geometry.as_mut()
                        }
                        .ok_or_else(|| {
                            ParseError::Structure("mxPoint found but no current mxGeometry".into())
                        })?;
                        let point = parse_mxpoint(&reader, &e)?;
                        assign_geometry_point(geom, point, in_points_array);
                    }
                    _ => {}
                }
            }

            Event::Text(t) => {
                if user_object_depth > 0 {
                    continue;
                }
                // Allowed "trimming": ignore indentation-only text nodes
                let txt = t.decode()?.into_owned();
                if txt.trim().is_empty() {
                    // formatting-only
                } else if let Some(d) = current_diagram.as_mut() {
                    // Preserve EXACT content (no trimming) if this diagram isn't parsed as mxGraphModel.
                    // This is where encoded payload lives in most .drawio files.
                    if d.graph_model.is_none() {
                        match d.encoded_payload.as_mut() {
                            Some(existing) => existing.push_str(&txt),
                            None => d.encoded_payload = Some(txt),
                        }
                    }
                }
            }

            Event::CData(c) => {
                if user_object_depth > 0 {
                    continue;
                }
                // Same policy as Text
                let txt = c.decode()?.into_owned();
                if txt.trim().is_empty() {
                    // formatting-only
                } else if let Some(d) = current_diagram.as_mut()
                    && d.graph_model.is_none()
                {
                    match d.encoded_payload.as_mut() {
                        Some(existing) => existing.push_str(&txt),
                        None => d.encoded_payload = Some(txt),
                    }
                }
            }

            Event::End(e) => {
                let name = local_name_end(&e)?;
                if is_user_object(&name) {
                    user_object_depth = user_object_depth.saturating_sub(1);
                    if user_object_depth == 0 {
                        current_user_object = None;
                    }
                    continue;
                }
                match name.as_str() {
                    "mxGraphModel" => {
                        let gm = current_graph.take().ok_or_else(|| {
                            ParseError::Structure("closing mxGraphModel but none open".into())
                        })?;
                        let d = current_diagram.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxGraphModel outside diagram".into())
                        })?;
                        d.graph_model = Some(gm);
                    }
                    "diagram" => {
                        let d = current_diagram.take().ok_or_else(|| {
                            ParseError::Structure("closing diagram but none open".into())
                        })?;
                        let d = decode_diagram_if_needed(d)?;
                        let file = mxfile.as_mut().ok_or_else(|| {
                            ParseError::Structure("diagram outside mxfile".into())
                        })?;
                        file.diagrams.push(d);
                    }
                    "mxCell" => {
                        current_cell_idx = None;
                        current_cell_is_user_object = false;
                    }
                    "mxGeometry" => {
                        current_geometry_cell_idx = None;
                        current_geometry_cell_is_user_object = false;
                        in_points_array = false;
                    }
                    "Array" => {
                        in_points_array = false;
                    }
                    _ => {}
                }
            }

            Event::Eof => break,
            _ => {}
        }

        buf.clear();
    }

    mxfile.ok_or_else(|| ParseError::Structure("no <mxfile> root element found".into()))
}

fn decode_diagram_if_needed(mut diagram: Diagram) -> ParseResult<Diagram> {
    if diagram.graph_model.is_none()
        && let Some(payload) = diagram.encoded_payload.as_deref()
    {
        let decoded = decode_diagram_payload(payload)?;
        diagram.graph_model = Some(parse_graph_model(&decoded)?);
    }
    Ok(diagram)
}

fn decode_diagram_payload(payload: &str) -> ParseResult<String> {
    let compact: String = payload.chars().filter(|c| !c.is_whitespace()).collect();
    let decoded = STANDARD.decode(compact)?;
    let mut decoder = DeflateDecoder::new(&decoded[..]);
    let mut inflated = Vec::new();
    decoder.read_to_end(&mut inflated)?;
    let inflated_str = std::str::from_utf8(&inflated)?;
    let decoded = percent_decode_str(inflated_str).decode_utf8()?;
    Ok(decoded.into_owned())
}

fn parse_graph_model(xml: &str) -> ParseResult<MxGraphModel> {
    let mut reader = Reader::from_str(xml);
    let mut buf = Vec::new();
    let mut current_graph: Option<MxGraphModel> = None;
    let mut current_cell_idx: Option<usize> = None;
    let mut current_geometry_cell_idx: Option<usize> = None;
    let mut in_points_array: bool = false;
    let mut user_object_depth: usize = 0;
    let mut current_user_object: Option<UserObjectContext> = None;
    let mut current_cell_is_user_object: bool = false;
    let mut current_geometry_cell_is_user_object: bool = false;

    loop {
        match reader.read_event_into(&mut buf)? {
            Event::Start(e) => {
                let name = local_name_start(&e)?;
                if is_user_object(&name) {
                    user_object_depth = user_object_depth.saturating_add(1);
                    if user_object_depth == 1 {
                        current_user_object = Some(parse_user_object_ctx(&e)?);
                    }
                    continue;
                }
                match name.as_str() {
                    "mxGraphModel" => {
                        if current_graph.is_some() {
                            return Err(ParseError::Structure(
                                "multiple mxGraphModel roots found".into(),
                            ));
                        }
                        let attrs = attrs_to_map(&e)?;

                        let mut gm = MxGraphModel {
                            dx: parse_i64_opt(attrs.get("dx"), "dx")?,
                            dy: parse_i64_opt(attrs.get("dy"), "dy")?,
                            grid: parse_bool_opt(attrs.get("grid")),
                            grid_size: parse_i64_opt(attrs.get("gridSize"), "gridSize")?,
                            guides: parse_bool_opt(attrs.get("guides")),
                            tooltips: parse_bool_opt(attrs.get("tooltips")),
                            connect: parse_bool_opt(attrs.get("connect")),
                            arrows: parse_bool_opt(attrs.get("arrows")),
                            fold: parse_bool_opt(attrs.get("fold")),
                            page: parse_bool_opt(attrs.get("page")),
                            page_scale: parse_f64_opt(attrs.get("pageScale"), "pageScale")?,
                            page_width: parse_f64_opt(attrs.get("pageWidth"), "pageWidth")?,
                            page_height: parse_f64_opt(attrs.get("pageHeight"), "pageHeight")?,
                            math: parse_bool_opt(attrs.get("math")),
                            shadow: parse_bool_opt(attrs.get("shadow")),
                            extra: BTreeMap::new(),
                            root: Root { cells: Vec::new() },
                            user_objects: Vec::new(),
                        };

                        for (k, v) in attrs {
                            if !is_known_graphmodel_attr(&k) {
                                gm.extra.insert(k, v);
                            }
                        }

                        current_graph = Some(gm);
                    }
                    "mxCell" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxCell outside mxGraphModel".into())
                        })?;
                        if user_object_depth > 0 {
                            let ctx = current_user_object.as_ref();
                            let cell = parse_mxcell_with_fallback(
                                &reader,
                                &e,
                                ctx.and_then(|c| c.id.as_deref()),
                                ctx.and_then(|c| c.label.as_deref()),
                            )?;
                            gm.user_objects.push(cell);
                            current_cell_idx = Some(gm.user_objects.len() - 1);
                            current_cell_is_user_object = true;
                        } else {
                            let cell = parse_mxcell(&reader, &e)?;
                            gm.root.cells.push(cell);
                            current_cell_idx = Some(gm.root.cells.len() - 1);
                            current_cell_is_user_object = false;
                        }
                    }
                    "mxGeometry" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxGeometry outside mxGraphModel".into())
                        })?;
                        let geom = parse_mxgeometry(&reader, &e)?;
                        let idx = current_cell_idx.ok_or_else(|| {
                            ParseError::Structure("mxGeometry found but no current mxCell".into())
                        })?;
                        if current_cell_is_user_object {
                            gm.user_objects[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = true;
                        } else {
                            gm.root.cells[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = false;
                        }
                        current_geometry_cell_idx = Some(idx);
                    }
                    "Array" => {
                        if current_geometry_cell_idx.is_some() && attr_is(&e, "as", "points")? {
                            in_points_array = true;
                        }
                    }
                    "mxPoint" => {
                        let idx = match current_geometry_cell_idx {
                            Some(idx) => idx,
                            None => continue,
                        };
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxPoint outside mxGraphModel".into())
                        })?;
                        let geom = if current_geometry_cell_is_user_object {
                            gm.user_objects[idx].geometry.as_mut()
                        } else {
                            gm.root.cells[idx].geometry.as_mut()
                        }
                        .ok_or_else(|| {
                            ParseError::Structure("mxPoint found but no current mxGeometry".into())
                        })?;
                        let point = parse_mxpoint(&reader, &e)?;
                        assign_geometry_point(geom, point, in_points_array);
                    }
                    _ => {}
                }
            }

            Event::Empty(e) => {
                let name = local_name_start(&e)?;
                if is_user_object(&name) {
                    continue;
                }
                match name.as_str() {
                    "mxCell" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxCell outside mxGraphModel".into())
                        })?;
                        if user_object_depth > 0 {
                            let ctx = current_user_object.as_ref();
                            let cell = parse_mxcell_with_fallback(
                                &reader,
                                &e,
                                ctx.and_then(|c| c.id.as_deref()),
                                ctx.and_then(|c| c.label.as_deref()),
                            )?;
                            gm.user_objects.push(cell);
                        } else {
                            let cell = parse_mxcell(&reader, &e)?;
                            gm.root.cells.push(cell);
                        }
                    }
                    "mxGeometry" => {
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxGeometry outside mxGraphModel".into())
                        })?;
                        let geom = parse_mxgeometry(&reader, &e)?;
                        let idx = current_cell_idx.ok_or_else(|| {
                            ParseError::Structure("mxGeometry found but no current mxCell".into())
                        })?;
                        if current_cell_is_user_object {
                            gm.user_objects[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = true;
                        } else {
                            gm.root.cells[idx].geometry = Some(geom);
                            current_geometry_cell_is_user_object = false;
                        }
                        current_geometry_cell_idx = Some(idx);
                    }
                    "Array" => {
                        if current_geometry_cell_idx.is_some() && attr_is(&e, "as", "points")? {
                            in_points_array = true;
                        }
                    }
                    "mxPoint" => {
                        let idx = match current_geometry_cell_idx {
                            Some(idx) => idx,
                            None => continue,
                        };
                        let gm = current_graph.as_mut().ok_or_else(|| {
                            ParseError::Structure("mxPoint outside mxGraphModel".into())
                        })?;
                        let geom = if current_geometry_cell_is_user_object {
                            gm.user_objects[idx].geometry.as_mut()
                        } else {
                            gm.root.cells[idx].geometry.as_mut()
                        }
                        .ok_or_else(|| {
                            ParseError::Structure("mxPoint found but no current mxGeometry".into())
                        })?;
                        let point = parse_mxpoint(&reader, &e)?;
                        assign_geometry_point(geom, point, in_points_array);
                    }
                    _ => {}
                }
            }

            Event::End(e) => {
                let name = local_name_end(&e)?;
                if is_user_object(&name) {
                    user_object_depth = user_object_depth.saturating_sub(1);
                    if user_object_depth == 0 {
                        current_user_object = None;
                    }
                    continue;
                }
                match name.as_str() {
                    "mxGraphModel" => {
                        return current_graph.take().ok_or_else(|| {
                            ParseError::Structure("closing mxGraphModel but none open".into())
                        });
                    }
                    "mxCell" => {
                        current_cell_idx = None;
                        current_cell_is_user_object = false;
                    }
                    "mxGeometry" => {
                        current_geometry_cell_idx = None;
                        current_geometry_cell_is_user_object = false;
                        in_points_array = false;
                    }
                    "Array" => {
                        in_points_array = false;
                    }
                    _ => {}
                }
            }

            Event::Eof => break,
            _ => {}
        }
        buf.clear();
    }

    Err(ParseError::Structure(
        "no <mxGraphModel> root element found".into(),
    ))
}

#[derive(Debug, Clone)]
struct UserObjectContext {
    id: Option<String>,
    label: Option<String>,
}

fn parse_user_object_ctx(e: &BytesStart<'_>) -> ParseResult<UserObjectContext> {
    let attrs = attrs_to_map(e)?;
    Ok(UserObjectContext {
        id: attrs.get("id").cloned(),
        label: attrs.get("label").cloned(),
    })
}

fn parse_mxcell<R: BufRead>(_reader: &Reader<R>, e: &BytesStart<'_>) -> ParseResult<MxCell> {
    let attrs = attrs_to_map(e)?;
    let id = attrs
        .get("id")
        .cloned()
        .ok_or(ParseError::MissingAttr("mxCell@id"))?;

    let mut extra = BTreeMap::new();
    for (k, v) in &attrs {
        if !is_known_cell_attr(k) {
            extra.insert(k.clone(), v.clone());
        }
    }

    Ok(MxCell {
        id,
        parent: attrs.get("parent").cloned(),
        source: attrs.get("source").cloned(),
        target: attrs.get("target").cloned(),
        value: attrs.get("value").cloned(),
        style: attrs.get("style").cloned(),
        vertex: parse_bool_opt(attrs.get("vertex")),
        edge: parse_bool_opt(attrs.get("edge")),
        extra,
        geometry: None,
        order: 0,
    })
}

fn parse_mxcell_with_fallback<R: BufRead>(
    _reader: &Reader<R>,
    e: &BytesStart<'_>,
    fallback_id: Option<&str>,
    fallback_value: Option<&str>,
) -> ParseResult<MxCell> {
    let attrs = attrs_to_map(e)?;
    let id = attrs
        .get("id")
        .cloned()
        .or_else(|| fallback_id.map(|value| value.to_string()))
        .ok_or(ParseError::MissingAttr("mxCell@id"))?;

    let mut extra = BTreeMap::new();
    for (k, v) in &attrs {
        if !is_known_cell_attr(k) {
            extra.insert(k.clone(), v.clone());
        }
    }

    Ok(MxCell {
        id,
        parent: attrs.get("parent").cloned(),
        source: attrs.get("source").cloned(),
        target: attrs.get("target").cloned(),
        value: attrs
            .get("value")
            .cloned()
            .or_else(|| fallback_value.map(|value| value.to_string())),
        style: attrs.get("style").cloned(),
        vertex: parse_bool_opt(attrs.get("vertex")),
        edge: parse_bool_opt(attrs.get("edge")),
        extra,
        geometry: None,
        order: 0,
    })
}

fn parse_mxgeometry<R: BufRead>(
    _reader: &Reader<R>,
    e: &BytesStart<'_>,
) -> ParseResult<MxGeometry> {
    let attrs = attrs_to_map(e)?;

    let mut extra = BTreeMap::new();
    for (k, v) in &attrs {
        if !is_known_geometry_attr(k) {
            extra.insert(k.clone(), v.clone());
        }
    }

    Ok(MxGeometry {
        x: parse_f64_opt(attrs.get("x"), "x")?,
        y: parse_f64_opt(attrs.get("y"), "y")?,
        width: parse_f64_opt(attrs.get("width"), "width")?,
        height: parse_f64_opt(attrs.get("height"), "height")?,
        relative: parse_bool_opt(attrs.get("relative")),
        as_attr: attrs.get("as").cloned(),
        extra,
        source_point: None,
        target_point: None,
        offset_point: None,
        points: Vec::new(),
        raw_x: attrs
            .get("x")
            .and_then(|value| value.trim().parse::<f64>().ok()),
        raw_y: attrs
            .get("y")
            .and_then(|value| value.trim().parse::<f64>().ok()),
    })
}

fn parse_mxpoint<R: BufRead>(_reader: &Reader<R>, e: &BytesStart<'_>) -> ParseResult<MxPoint> {
    let attrs = attrs_to_map(e)?;
    Ok(MxPoint {
        x: parse_f64_opt(attrs.get("x"), "x")?,
        y: parse_f64_opt(attrs.get("y"), "y")?,
        as_attr: attrs.get("as").cloned(),
    })
}

fn assign_geometry_point(geom: &mut MxGeometry, point: MxPoint, in_points_array: bool) {
    match point.as_attr.as_deref() {
        Some("sourcePoint") => geom.source_point = Some(point),
        Some("targetPoint") => geom.target_point = Some(point),
        Some("offset") => geom.offset_point = Some(point),
        _ => {
            if in_points_array {
                geom.points.push(point);
            }
        }
    }
}

fn attr_is(e: &BytesStart<'_>, key: &str, expected: &str) -> ParseResult<bool> {
    for a in e.attributes() {
        let a = a?;
        if a.key.as_ref() != key.as_bytes() {
            continue;
        }
        return Ok(a.unescape_value()?.as_ref() == expected);
    }
    Ok(false)
}

fn attrs_to_map(e: &BytesStart<'_>) -> ParseResult<BTreeMap<String, String>> {
    let mut out = BTreeMap::new();
    for a in e.attributes() {
        let a = a?; // AttrError -> ParseError via #[from]
        let key = str::from_utf8(a.key.as_ref())?.to_string();
        let val = a.unescape_value()?.to_string();
        out.insert(key, val);
    }
    Ok(out)
}

fn local_name_start(e: &BytesStart<'_>) -> ParseResult<String> {
    Ok(str::from_utf8(e.name().as_ref())?.to_string())
}

fn local_name_end(e: &BytesEnd<'_>) -> ParseResult<String> {
    Ok(str::from_utf8(e.name().as_ref())?.to_string())
}

fn is_user_object(name: &str) -> bool {
    name.eq_ignore_ascii_case("userObject")
}

fn parse_bool_opt(v: Option<&String>) -> Option<bool> {
    let s = v?;
    match s.as_str() {
        "1" | "true" | "TRUE" | "True" => Some(true),
        "0" | "false" | "FALSE" | "False" => Some(false),
        _ => None,
    }
}

fn parse_i64_opt(v: Option<&String>, field: &'static str) -> ParseResult<Option<i64>> {
    let Some(s) = v else { return Ok(None) };
    if s.trim().is_empty() {
        return Ok(None);
    }
    let parsed = s.parse::<i64>().map_err(|_| ParseError::InvalidNumber {
        field,
        value: s.clone(),
    })?;
    Ok(Some(parsed))
}

fn parse_f64_opt(v: Option<&String>, field: &'static str) -> ParseResult<Option<f64>> {
    let Some(s) = v else { return Ok(None) };
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }
    let parsed = serde_json::from_str::<f64>(trimmed).map_err(|_| ParseError::InvalidNumber {
        field,
        value: s.clone(),
    })?;
    Ok(Some(parsed))
}

fn is_known_graphmodel_attr(k: &str) -> bool {
    matches!(
        k,
        "dx" | "dy"
            | "grid"
            | "gridSize"
            | "guides"
            | "tooltips"
            | "connect"
            | "arrows"
            | "fold"
            | "page"
            | "pageScale"
            | "pageWidth"
            | "pageHeight"
            | "math"
            | "shadow"
    )
}

fn is_known_cell_attr(k: &str) -> bool {
    matches!(
        k,
        "id" | "parent" | "source" | "target" | "value" | "style" | "vertex" | "edge"
    )
}

fn is_known_geometry_attr(k: &str) -> bool {
    matches!(k, "x" | "y" | "width" | "height" | "relative" | "as")
}
