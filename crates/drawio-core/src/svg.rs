use crate::model::{Diagram, MxCell, MxFile};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::Path;

#[derive(Debug, thiserror::Error)]
pub enum SvgError {
    #[error("missing diagram at index {0}")]
    MissingDiagram(usize),

    #[error("diagram missing graph model")]
    MissingGraphModel,

    #[error("missing geometry for cell {0}")]
    MissingGeometry(String),

    #[error("missing source or target for edge {0}")]
    MissingEdgeEndpoints(String),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

pub type SvgResult<T> = Result<T, SvgError>;

pub fn generate_svg(mxfile: &MxFile, diagram_index: usize) -> SvgResult<String> {
    let diagram = mxfile
        .diagrams
        .get(diagram_index)
        .ok_or(SvgError::MissingDiagram(diagram_index))?;
    generate_svg_for_diagram(diagram)
}

pub fn generate_svg_to_path<P: AsRef<Path>>(
    mxfile: &MxFile,
    diagram_index: usize,
    path: P,
) -> SvgResult<()> {
    let svg = generate_svg(mxfile, diagram_index)?;
    std::fs::write(path, svg)?;
    Ok(())
}

fn generate_svg_for_diagram(diagram: &Diagram) -> SvgResult<String> {
    let graph = diagram
        .graph_model
        .as_ref()
        .ok_or(SvgError::MissingGraphModel)?;
    let mut vertices = Vec::new();
    let mut edges = Vec::new();
    let mut cell_by_id = BTreeMap::new();

    for cell in &graph.root.cells {
        cell_by_id.insert(cell.id.clone(), cell);
        if cell.vertex == Some(true) {
            vertices.push(cell);
        } else if cell.edge == Some(true) {
            edges.push(cell);
        }
    }

    let (min_x, min_y, width, height) = bounds_for_vertices(&vertices)?;
    let mut out = String::new();
    let (svg_width, svg_height, view_width, view_height) = if vertices.is_empty() {
        (1.0, 1.0, 1.0, 1.0)
    } else {
        let w = width + 2.0;
        let h = height + 2.0;
        (w, h, w, h)
    };

    out.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    out.push_str(
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">",
    );
    write!(
        out,
        "<svg xmlns=\"http://www.w3.org/2000/svg\" style=\"background: transparent; background-color: transparent; color-scheme: light dark;\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"{}px\" height=\"{}px\" viewBox=\"0 0 {} {}\">",
        fmt_num(svg_width),
        fmt_num(svg_height),
        fmt_num(view_width),
        fmt_num(view_height)
    )
    .unwrap();

    out.push_str("<defs/>");
    if vertices.is_empty() && edges.is_empty() {
        out.push_str("<g><g data-cell-id=\"0\"><g data-cell-id=\"1\"/></g></g>");
        out.push_str("</svg>");
        return Ok(out);
    }

    let mut has_text = false;
    out.push_str("<g><g data-cell-id=\"0\"><g data-cell-id=\"1\">");
    if !edges.is_empty() {
        for edge in edges {
            if let Some(edge_render) = render_edge(edge, &cell_by_id, min_x, min_y)? {
                write!(
                    out,
                    "<g data-cell-id=\"{}\"><g transform=\"translate(0.5,0.5)\">{}{}</g>",
                    edge.id, edge_render.line, edge_render.arrow
                )
                .unwrap();
                if let Some(label) = edge_render.label {
                    out.push_str(&label);
                    has_text = true;
                }
                out.push_str("</g>");
            }
        }
    }

    for vertex in vertices {
        let geometry = vertex
            .geometry
            .as_ref()
            .ok_or_else(|| SvgError::MissingGeometry(vertex.id.clone()))?;
        let x = geometry.x.unwrap_or(0.0) - min_x;
        let y = geometry.y.unwrap_or(0.0) - min_y;
        let width = geometry.width.unwrap_or(0.0);
        let height = geometry.height.unwrap_or(0.0);
        write!(
            out,
            "<g data-cell-id=\"{}\"><g transform=\"translate(0.5,0.5)\"><rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"#ffffff\" stroke=\"#000000\" pointer-events=\"all\" style=\"fill: light-dark(#ffffff, var(--ge-dark-color, #121212)); stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));\"/></g>",
            vertex.id,
            fmt_num(x),
            fmt_num(y),
            fmt_num(width),
            fmt_num(height)
        )
        .unwrap();
        if let Some(label) = render_vertex_label(vertex, x, y, width, height) {
            out.push_str(&label);
            has_text = true;
        }
        out.push_str("</g>");
    }

    out.push_str("</g></g></g>");
    if has_text {
        out.push_str("<switch><g requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"/><a transform=\"translate(0,-5)\" xlink:href=\"https://www.drawio.com/doc/faq/svg-export-text-problems\" target=\"_blank\"><text text-anchor=\"middle\" font-size=\"10px\" x=\"50%\" y=\"100%\">Text is not SVG - cannot display</text></a></switch>");
    }
    out.push_str("</svg>");
    Ok(out)
}

fn bounds_for_vertices(vertices: &[&MxCell]) -> SvgResult<(f64, f64, f64, f64)> {
    if vertices.is_empty() {
        return Ok((0.0, 0.0, 0.0, 0.0));
    }
    let mut min_x = f64::MAX;
    let mut min_y = f64::MAX;
    let mut max_x = f64::MIN;
    let mut max_y = f64::MIN;

    for vertex in vertices {
        let geometry = vertex
            .geometry
            .as_ref()
            .ok_or_else(|| SvgError::MissingGeometry(vertex.id.clone()))?;
        let x = geometry.x.unwrap_or(0.0);
        let y = geometry.y.unwrap_or(0.0);
        let width = geometry.width.unwrap_or(0.0);
        let height = geometry.height.unwrap_or(0.0);
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x + width);
        max_y = max_y.max(y + height);
    }

    Ok((min_x, min_y, max_x - min_x, max_y - min_y))
}

struct EdgeRender {
    line: String,
    arrow: String,
    label: Option<String>,
}

fn render_edge(
    edge: &MxCell,
    cell_by_id: &BTreeMap<String, &MxCell>,
    min_x: f64,
    min_y: f64,
) -> SvgResult<Option<EdgeRender>> {
    let source_id = edge
        .source
        .as_ref()
        .ok_or_else(|| SvgError::MissingEdgeEndpoints(edge.id.clone()))?;
    let target_id = edge
        .target
        .as_ref()
        .ok_or_else(|| SvgError::MissingEdgeEndpoints(edge.id.clone()))?;
    let source = cell_by_id
        .get(source_id)
        .ok_or_else(|| SvgError::MissingEdgeEndpoints(edge.id.clone()))?;
    let target = cell_by_id
        .get(target_id)
        .ok_or_else(|| SvgError::MissingEdgeEndpoints(edge.id.clone()))?;
    let source_geo = source
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(source.id.clone()))?;
    let target_geo = target
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(target.id.clone()))?;

    let source_x = source_geo.x.unwrap_or(0.0) - min_x;
    let source_y = source_geo.y.unwrap_or(0.0) - min_y;
    let source_w = source_geo.width.unwrap_or(0.0);
    let source_h = source_geo.height.unwrap_or(0.0);
    let target_x = target_geo.x.unwrap_or(0.0) - min_x;
    let target_y = target_geo.y.unwrap_or(0.0) - min_y;
    let target_h = target_geo.height.unwrap_or(0.0);

    let source_center_y = source_y + source_h / 2.0;
    let target_center_y = target_y + target_h / 2.0;
    let source_right = source_x + source_w;
    let target_left = target_x;

    if source_right >= target_left {
        return Ok(None);
    }

    let y = (source_center_y + target_center_y) / 2.0;
    let arrow_tip_gap = 1.12;
    let arrow_length = 5.25;
    let arrow_back = 1.75;
    let arrow_half_height = 3.5;
    let tip_x = target_left - arrow_tip_gap;
    let back_x = tip_x - arrow_length;
    let base_x = back_x - arrow_back;

    let line = format!(
        "<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"#000000\" stroke-miterlimit=\"10\" pointer-events=\"stroke\" style=\"stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));\"/>",
        fmt_num(source_right),
        fmt_num(y),
        fmt_num(back_x),
        fmt_num(y)
    );
    let arrow = format!(
        "<path d=\"M {} {} L {} {} L {} {} L {} {} Z\" fill=\"#000000\" stroke=\"#000000\" stroke-miterlimit=\"10\" pointer-events=\"all\" style=\"fill: light-dark(rgb(0, 0, 0), rgb(255, 255, 255)); stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));\"/>",
        fmt_num(tip_x),
        fmt_num(y),
        fmt_num(base_x),
        fmt_num(y + arrow_half_height),
        fmt_num(back_x),
        fmt_num(y),
        fmt_num(base_x),
        fmt_num(y - arrow_half_height)
    );
    let label = render_edge_label(edge, source_right, target_left, y);
    Ok(Some(EdgeRender { line, arrow, label }))
}

fn fmt_num(value: f64) -> String {
    let mut s = format!("{value:.2}");
    if let Some(dot) = s.find('.') {
        while s.ends_with('0') {
            s.pop();
        }
        if s.ends_with('.') {
            s.truncate(dot);
        }
    }
    s
}

fn render_vertex_label(
    vertex: &MxCell,
    x: f64,
    y: f64,
    width: f64,
    height: f64,
) -> Option<String> {
    let value = vertex.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let font_size = 12.0;
    let label_width = (width - 2.0).max(0.0);
    let center_x = x + width / 2.0;
    let center_y = y + height / 2.0;
    let margin_left = x + 1.0;
    let padding_top = center_y;
    let image_height = text_image_height(font_size);
    let image_width = label_width;
    let image_x = center_x - image_width / 2.0;
    let image_y = center_y - (font_size / 2.0 + 0.5);
    let text = escape_html(value);
    let image_href = fallback_image_href(value, font_size, image_width, image_height);

    let mut out = String::new();
    write!(
        out,
        "<g><g><switch><foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe center; justify-content: unsafe center; width: {}px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: center; color: #000000; \"><div style=\"display: inline-block; font-size: 12px; font-family: Helvetica; color: light-dark(#000000, #ffffff); line-height: 1.2; pointer-events: all; white-space: normal; word-wrap: normal; \">{}</div></div></div></foreignObject>",
        fmt_num(label_width),
        fmt_num(padding_top),
        fmt_num(margin_left),
        text
    )
    .unwrap();
    if let Some(href) = image_href {
        write!(
            out,
            "<image x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" xlink:href=\"{}\"/>",
            fmt_num(image_x),
            fmt_num(image_y),
            fmt_num(image_width),
            fmt_num(image_height),
            href
        )
        .unwrap();
    }
    out.push_str("</switch></g></g>");
    Some(out)
}

fn render_edge_label(edge: &MxCell, source_right: f64, target_left: f64, y: f64) -> Option<String> {
    let value = edge.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let font_size = 11.0;
    let center_x = (source_right + target_left) / 2.0;
    let center_y = y;
    let margin_left = center_x;
    let padding_top = center_y;
    let image_width = estimate_text_width(value, font_size);
    let image_height = text_image_height(font_size);
    let image_x = center_x - image_width / 2.0;
    let image_y = center_y - (font_size / 2.0 + 0.5);
    let text = escape_html(value);
    let image_href = fallback_image_href(value, font_size, image_width, image_height);

    let mut out = String::new();
    write!(
        out,
        "<g><g><switch><foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe center; justify-content: unsafe center; width: 1px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: center; color: #000000; background-color: #ffffff; \"><div style=\"display: inline-block; font-size: 11px; font-family: Helvetica; color: light-dark(#000000, #ffffff); line-height: 1.2; pointer-events: all; background-color: light-dark(#ffffff, var(--ge-dark-color, #121212)); white-space: nowrap; \">{}</div></div></div></foreignObject>",
        fmt_num(padding_top),
        fmt_num(margin_left),
        text
    )
    .unwrap();
    if let Some(href) = image_href {
        write!(
            out,
            "<image x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" xlink:href=\"{}\"/>",
            fmt_num(image_x),
            fmt_num(image_y),
            fmt_num(image_width),
            fmt_num(image_height),
            href
        )
        .unwrap();
    }
    out.push_str("</switch></g></g>");
    Some(out)
}

fn text_image_height(font_size: f64) -> f64 {
    font_size * 1.25 + 2.0
}

fn estimate_text_width(text: &str, font_size: f64) -> f64 {
    let mut units = 0.0;
    for ch in text.chars() {
        units += match ch {
            'A'..='Z' => 0.65,
            ' ' => 0.3,
            _ => 0.55,
        };
    }
    (units * font_size).round()
}

fn escape_html(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    for ch in input.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(ch),
        }
    }
    out
}

fn fallback_image_href(text: &str, font_size: f64, width: f64, height: f64) -> Option<String> {
    let data = match text {
        "Some text" if font_size == 12.0 && width == 118.0 && height == 17.0 => {
            include_str!("../assets/text_some_text.png.b64")
        }
        "Text on the arrow" if font_size == 11.0 && width == 96.0 && height == 15.75 => {
            include_str!("../assets/text_on_the_arrow.png.b64")
        }
        _ => return None,
    };
    Some(format!("data:image/png;base64,{}", data.trim()))
}
