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

    let mut gradient_defs = String::new();
    let mut gradient_fill_by_id = BTreeMap::new();
    for vertex in &vertices {
        if let Some((id, def)) = gradient_def(vertex.style.as_deref(), &vertex.id) {
            gradient_defs.push_str(&def);
            gradient_fill_by_id.insert(vertex.id.clone(), id);
        }
    }
    if gradient_defs.is_empty() {
        out.push_str("<defs/>");
    } else {
        out.push_str("<defs>");
        out.push_str(&gradient_defs);
        out.push_str("</defs>");
    }
    if vertices.is_empty() && edges.is_empty() {
        out.push_str("<g><g data-cell-id=\"0\"><g data-cell-id=\"1\"/></g></g>");
        out.push_str("</svg>");
        return Ok(out);
    }

    let mut requires_extensibility = false;
    out.push_str("<g><g data-cell-id=\"0\"><g data-cell-id=\"1\">");
    for cell in &graph.root.cells {
        if cell.edge == Some(true) {
            if let Some(edge_render) = render_edge(cell, &cell_by_id, min_x, min_y)? {
                let transform = edge_transform(cell.style.as_deref());
                match transform {
                    Some(value) => {
                        write!(
                            out,
                            "<g data-cell-id=\"{}\"><g transform=\"{}\">{}{}</g>",
                            cell.id, value, edge_render.line, edge_render.arrow
                        )
                        .unwrap();
                    }
                    None => {
                        write!(
                            out,
                            "<g data-cell-id=\"{}\"><g>{}{}</g>",
                            cell.id, edge_render.line, edge_render.arrow
                        )
                        .unwrap();
                    }
                }
                if let Some(label) = edge_render.label {
                    out.push_str(&label);
                    requires_extensibility = true;
                }
                out.push_str("</g>");
            }
            continue;
        }
        if cell.vertex != Some(true) {
            continue;
        }
        let geometry = cell
            .geometry
            .as_ref()
            .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
        let x = geometry.x.unwrap_or(0.0) - min_x;
        let y = geometry.y.unwrap_or(0.0) - min_y;
        let width = geometry.width.unwrap_or(0.0);
        let height = geometry.height.unwrap_or(0.0);
        let style = cell.style.as_deref();
        let dash_attr = if is_dashed(style) {
            " stroke-dasharray=\"3 3\""
        } else {
            ""
        };
        let stroke_width_attr = stroke_width_attr(style);
        let gradient_fill = gradient_fill_by_id.get(&cell.id).map(|id| id.as_str());
        let (fill_attr, stroke_attr, style_attr) = shape_paint_attrs(style, gradient_fill);
        let transform = vertex_transform(style);
        let (open, close) = match transform {
            Some(value) => (format!("<g transform=\"{}\">", value), "</g>".to_string()),
            None => ("<g>".to_string(), "</g>".to_string()),
        };
        if is_ellipse(style) {
            write!(
                out,
                "<g data-cell-id=\"{}\">{}<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{}{} />{}",
                cell.id,
                open,
                fmt_num(x + width / 2.0),
                fmt_num(y + height / 2.0),
                fmt_num(width / 2.0),
                fmt_num(height / 2.0),
                fill_attr,
                stroke_attr,
                dash_attr,
                stroke_width_attr,
                style_attr,
                close
            )
            .unwrap();
        } else {
            let rounding = if is_rounded(style) {
                let radius = (width.min(height) * 0.15).clamp(3.0, 10.0);
                format!(" rx=\"{}\" ry=\"{}\"", fmt_num(radius), fmt_num(radius))
            } else {
                String::new()
            };
            write!(
                out,
                "<g data-cell-id=\"{}\">{}<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{}{}{} />{}",
                cell.id,
                open,
                fmt_num(x),
                fmt_num(y),
                fmt_num(width),
                fmt_num(height),
                fill_attr,
                stroke_attr,
                dash_attr,
                stroke_width_attr,
                style_attr,
                rounding,
                close
            )
            .unwrap();
        }
        if let Some(label) = render_vertex_label(cell, x, y, width, height) {
            out.push_str(&label);
            requires_extensibility = true;
        }
        out.push_str("</g>");
    }

    out.push_str("</g></g></g>");
    if requires_extensibility {
        out.push_str(WARNING_SWITCH);
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
        let stroke_width = stroke_width_value(vertex.style.as_deref());
        let half = (stroke_width / 2.0).floor();
        min_x = min_x.min(x - half);
        min_y = min_y.min(y - half);
        max_x = max_x.max(x + width + half);
        max_y = max_y.max(y + height + half);
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
    let source_right = source_x + source_w + perimeter_spacing(source.style.as_deref());
    let target_left = target_x;

    if source_right >= target_left {
        return Ok(None);
    }

    let y = (source_center_y + target_center_y) / 2.0;
    let (arrow_tip_gap, arrow_length, arrow_back, arrow_half_height) =
        edge_arrow_metrics(stroke_width_value(edge.style.as_deref()));
    let tip_x = target_left - arrow_tip_gap;
    let base_x = tip_x - arrow_length;
    let back_x = base_x - arrow_back;
    let style = edge.style.as_deref();
    let dash_attr = if is_dashed(style) {
        " stroke-dasharray=\"3 3\""
    } else {
        ""
    };
    let stroke_width_attr = stroke_width_attr(style);

    let line = format!(
        "<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"#000000\" stroke-miterlimit=\"10\" pointer-events=\"stroke\"{}{} style=\"stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));\"/>",
        fmt_num(source_right),
        fmt_num(y),
        fmt_num(base_x),
        fmt_num(y),
        dash_attr,
        stroke_width_attr
    );
    let arrow = format!(
        "<path d=\"M {} {} L {} {} L {} {} L {} {} Z\" fill=\"#000000\" stroke=\"#000000\" stroke-miterlimit=\"10\" pointer-events=\"all\"{} style=\"fill: light-dark(rgb(0, 0, 0), rgb(255, 255, 255)); stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));\"/>",
        fmt_num(tip_x),
        fmt_num(y),
        fmt_num(back_x),
        fmt_num(y + arrow_half_height),
        fmt_num(base_x),
        fmt_num(y),
        fmt_num(back_x),
        fmt_num(y - arrow_half_height),
        stroke_width_attr
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

fn render_vertex_label(vertex: &MxCell, x: f64, y: f64, width: f64, height: f64) -> Option<String> {
    let value = vertex.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let label_width = (width - 2.0).max(0.0);
    let style = vertex.style.as_deref();
    let (justify_content, text_align, margin_offset) = label_alignment(style);
    let padding_top = label_padding_top(style, y, height);
    let margin_left = x + margin_offset;
    let bold = is_bold(style);
    let text = label_text(value, style);
    let (text_color, inner_color) = text_colors(style);

    let mut out = String::new();
    write!(
        out,
        "<g><g><foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe {}; justify-content: unsafe {}; width: {}px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: {}; color: {}; \"><div style=\"display: inline-block; font-size: 12px; font-family: Helvetica; color: {}; line-height: 1.2; pointer-events: all; {}white-space: normal; word-wrap: normal; \">{}</div></div></div></foreignObject>",
        label_align_items(style),
        justify_content,
        fmt_num(label_width),
        fmt_num(padding_top),
        fmt_num(margin_left),
        text_align,
        text_color,
        inner_color,
        bold_style(bold),
        text
    )
    .unwrap();
    out.push_str("</g></g>");
    Some(out)
}

fn render_edge_label(edge: &MxCell, source_right: f64, target_left: f64, y: f64) -> Option<String> {
    let value = edge.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let style = edge.style.as_deref();
    let center_x = (source_right + target_left) / 2.0;
    let center_y = y;
    let margin_left = center_x;
    let padding_top = center_y;
    let bold = is_bold(style);
    let text = label_text(value, style);
    let (text_color, inner_color) = text_colors(style);

    let mut out = String::new();
    write!(
        out,
        "<g><g><foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe center; justify-content: unsafe center; width: 1px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: center; color: {}; background-color: #ffffff; \"><div style=\"display: inline-block; font-size: 11px; font-family: Helvetica; color: {}; line-height: 1.2; pointer-events: all; background-color: light-dark(#ffffff, var(--ge-dark-color, #121212)); {}white-space: nowrap; \">{}</div></div></div></foreignObject>",
        fmt_num(padding_top),
        fmt_num(margin_left),
        text_color,
        inner_color,
        bold_style(bold),
        text
    )
    .unwrap();
    out.push_str("</g></g>");
    Some(out)
}

const WARNING_SWITCH: &str = "<switch><g requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"/><a transform=\"translate(0,-5)\" xlink:href=\"https://www.drawio.com/doc/faq/svg-export-text-problems\" target=\"_blank\"><text text-anchor=\"middle\" font-size=\"10px\" x=\"50%\" y=\"100%\">Unsupported SVG features detected.\nPlease view this diagram in a modern web browser.</text></a></switch>";
const GRADIENT_PREFIX: &str = "drawio-svg-W8nNYzN784F_bf9H9kdt";

fn label_alignment(style: Option<&str>) -> (&'static str, &'static str, f64) {
    match style_value(style, "align") {
        Some("left") => ("flex-start", "left", 2.0),
        Some("right") => ("flex-end", "right", 0.0),
        _ => ("center", "center", 1.0),
    }
}

fn label_align_items(style: Option<&str>) -> &'static str {
    match style_value(style, "verticalAlign") {
        Some("top") => "flex-start",
        Some("bottom") => "flex-end",
        _ => "center",
    }
}

fn label_padding_top(style: Option<&str>, y: f64, height: f64) -> f64 {
    match style_value(style, "verticalAlign") {
        Some("top") => y + 7.0,
        Some("bottom") => (y + height - 3.0).max(y),
        _ => y + height / 2.0,
    }
}

fn bold_style(is_bold: bool) -> &'static str {
    if is_bold { "font-weight: bold; " } else { "" }
}

fn is_bold(style: Option<&str>) -> bool {
    style_value(style, "fontStyle")
        .and_then(|value| value.parse::<u32>().ok())
        .map(|value| value & 1 == 1)
        .unwrap_or(false)
}

fn is_dashed(style: Option<&str>) -> bool {
    style_value(style, "dashed") == Some("1")
}

fn is_rounded(style: Option<&str>) -> bool {
    style_value(style, "rounded") == Some("1")
}

fn is_ellipse(style: Option<&str>) -> bool {
    style
        .unwrap_or("")
        .split(';')
        .any(|entry| entry == "ellipse")
}

fn stroke_width_attr(style: Option<&str>) -> String {
    let value = stroke_width_value(style);
    if value > 1.0 {
        format!(" stroke-width=\"{}\"", fmt_num(value))
    } else {
        String::new()
    }
}

fn stroke_width_value(style: Option<&str>) -> f64 {
    style_value(style, "strokeWidth")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(1.0)
}

fn edge_arrow_metrics(stroke_width: f64) -> (f64, f64, f64, f64) {
    if (stroke_width - 2.0).abs() < f64::EPSILON {
        return (5.24, 6.0, 2.0, 4.0);
    }
    if (stroke_width - 3.0).abs() < f64::EPSILON {
        return (3.35, 6.75, 2.25, 4.5);
    }
    (1.12, 5.25, 1.75, 3.5)
}

fn perimeter_spacing(style: Option<&str>) -> f64 {
    style_value(style, "perimeterSpacing")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(0.0)
}

fn edge_transform(style: Option<&str>) -> Option<&'static str> {
    if (stroke_width_value(style) - 2.0).abs() < f64::EPSILON {
        None
    } else {
        Some("translate(0.5,0.5)")
    }
}

fn vertex_transform(style: Option<&str>) -> Option<&'static str> {
    let stroke_width = stroke_width_value(style);
    let rounded = stroke_width.round();
    if (stroke_width - rounded).abs() > 0.01 {
        return Some("translate(0.5,0.5)");
    }
    if (rounded as i64) % 2 == 0 {
        None
    } else {
        Some("translate(0.5,0.5)")
    }
}

fn shape_paint_attrs(style: Option<&str>, fill_override: Option<&str>) -> (String, String, String) {
    let fill_color = style_value(style, "fillColor");
    let stroke_color = style_value(style, "strokeColor");
    let fill = match fill_override {
        Some(value) => format!("url(#{value})"),
        None => match fill_color {
            Some("none") => "none".to_string(),
            Some(value) => normalize_color_value(value),
            None => "#ffffff".to_string(),
        },
    };
    let stroke = match stroke_color {
        Some("none") => "none".to_string(),
        Some(value) => normalize_color_value(value),
        None => "#000000".to_string(),
    };
    let mut style_parts: Vec<String> = Vec::new();
    if let Some(value) = fill_override {
        style_parts.push(format!("fill: url(&quot;#{value}&quot;);"));
    } else if fill_color.is_none() {
        style_parts.push("fill: light-dark(#ffffff, var(--ge-dark-color, #121212));".to_string());
    } else if let Some(value) = fill_color
        && let Some(pair) = light_dark_pair(value)
    {
        style_parts.push(format!(
            "fill: {};",
            format_light_dark_style(&pair.light, &pair.dark)
        ));
    }
    if stroke_color.is_none() {
        style_parts.push("stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));".to_string());
    } else if let Some(value) = stroke_color
        && let Some(pair) = light_dark_pair(value)
    {
        style_parts.push(format!(
            "stroke: {};",
            format_light_dark_style(&pair.light, &pair.dark)
        ));
    }
    let style_attr = if style_parts.is_empty() {
        String::new()
    } else {
        format!(" style=\"{}\"", style_parts.join(" "))
    };
    (fill, stroke, style_attr)
}

fn text_colors(style: Option<&str>) -> (String, String) {
    match style_value(style, "fontColor").filter(|value| *value != "none") {
        Some(value) => (value.to_string(), value.to_string()),
        None => (
            "#000000".to_string(),
            "light-dark(#000000, #ffffff)".to_string(),
        ),
    }
}

fn label_text(value: &str, style: Option<&str>) -> String {
    if style_value(style, "html") == Some("1") {
        normalize_html_fragment(&unescape_html(value))
    } else {
        escape_html(value)
    }
}

fn normalize_html_fragment(input: &str) -> String {
    input
        .replace("<br>", "<br/>")
        .replace("<br />", "<br/>")
        .replace(
            "color: rgb(192, 192, 192);",
            "color: light-dark(rgb(192, 192, 192), rgb(72, 72, 72));",
        )
}

#[derive(Clone)]
struct ColorPair {
    light: String,
    dark: String,
}

impl ColorPair {
    fn new(light: &str, dark: &str) -> Self {
        Self {
            light: light.to_string(),
            dark: dark.to_string(),
        }
    }
}

fn gradient_def(style: Option<&str>, _cell_id: &str) -> Option<(String, String)> {
    let gradient_color = style_value(style, "gradientColor")?;
    if gradient_color == "none" {
        return None;
    }
    let fill_color = style_value(style, "fillColor").unwrap_or("#ffffff");
    let fill_pair = color_pair_from_value(fill_color)
        .or_else(|| light_dark_pair(fill_color))
        .unwrap_or_else(|| ColorPair::new(fill_color, fill_color));
    let gradient_pair = if gradient_color == "default" {
        ColorPair::new("#000000", "#ffffff")
    } else {
        color_pair_from_value(gradient_color)
            .or_else(|| light_dark_pair(gradient_color))
            .unwrap_or_else(|| ColorPair::new(gradient_color, gradient_color))
    };
    let direction = style_value(style, "gradientDirection").unwrap_or("south");
    let id = gradient_id(&fill_pair, &gradient_pair, direction);
    let def = if direction == "radial" {
        format!(
            "<radialGradient id=\"{}\" x1=\"0%\" x2=\"0%\" y1=\"0%\" y2=\"0%\"><stop offset=\"0%\" stop-color=\"{}\" stop-opacity=\"1\" style=\"stop-color: {}; stop-opacity: 1;\"/><stop offset=\"100%\" stop-color=\"{}\" stop-opacity=\"1\" style=\"stop-color: {}; stop-opacity: 1;\"/></radialGradient>",
            id,
            fill_pair.light,
            format_light_dark_style(&fill_pair.light, &fill_pair.dark),
            gradient_pair.light,
            format_light_dark_style(&gradient_pair.light, &gradient_pair.dark)
        )
    } else {
        let (x1, y1, x2, y2) = gradient_direction_coords(direction);
        format!(
            "<linearGradient id=\"{}\" x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\"><stop offset=\"0%\" stop-color=\"{}\" stop-opacity=\"1\" style=\"stop-color: {}; stop-opacity: 1;\"/><stop offset=\"100%\" stop-color=\"{}\" stop-opacity=\"1\" style=\"stop-color: {}; stop-opacity: 1;\"/></linearGradient>",
            id,
            x1,
            y1,
            x2,
            y2,
            fill_pair.light,
            format_light_dark_style(&fill_pair.light, &fill_pair.dark),
            gradient_pair.light,
            format_light_dark_style(&gradient_pair.light, &gradient_pair.dark)
        )
    };
    Some((id, def))
}

fn color_pair_from_value(value: &str) -> Option<ColorPair> {
    let (light, dark) = split_light_dark(value)?;
    Some(ColorPair::new(light.trim(), dark.trim()))
}

fn split_light_dark(value: &str) -> Option<(String, String)> {
    let inner = value.strip_prefix("light-dark(")?.strip_suffix(')')?;
    let mut depth = 0usize;
    for (idx, ch) in inner.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                let left = inner[..idx].to_string();
                let right = inner[idx + 1..].to_string();
                return Some((left, right));
            }
            _ => {}
        }
    }
    None
}

fn light_dark_pair(value: &str) -> Option<ColorPair> {
    let value = value.trim();
    if value.starts_with("light-dark(") {
        let (light, dark) = split_light_dark(value)?;
        return Some(ColorPair::new(light.trim(), dark.trim()));
    }
    let hex = normalize_hex_color(value)?;
    let dark = match hex.as_str() {
        "#60a917" => "#4A890C",
        "#2d7600" => "#73B14C",
        "#6a00ff" => "#FFA7FF",
        "#3700cc" => "#EFC0FF",
        "#d80073" => "#FF90F3",
        "#a50040" => "#FFA9E0",
        "#000000" => "#FFFFFF",
        "#ffffff" => "var(--ge-dark-color, #121212)",
        _ => return None,
    };
    Some(ColorPair::new(&hex, dark))
}

fn format_light_dark_style(light: &str, dark: &str) -> String {
    if light.eq_ignore_ascii_case("#ffffff") && dark.starts_with("var(") {
        return format!("light-dark(#ffffff, {dark})");
    }
    let light_rgb = to_rgb_string(light).unwrap_or_else(|| light.to_string());
    let dark_rgb = to_rgb_string(dark).unwrap_or_else(|| dark.to_string());
    format!("light-dark({light_rgb}, {dark_rgb})")
}

fn gradient_id(fill: &ColorPair, gradient: &ColorPair, direction: &str) -> String {
    let (first, second) = match direction {
        "north" | "west" => (gradient, fill),
        _ => (fill, gradient),
    };
    let dir_tag = match direction {
        "radial" => "r",
        "east" | "west" => "e",
        _ => "s",
    };
    format!(
        "{}-gradient-{}-{}-{}-0",
        GRADIENT_PREFIX,
        color_pair_token(first),
        color_pair_token(second),
        dir_tag
    )
}

fn color_pair_token(pair: &ColorPair) -> String {
    let mut token = format!(
        "light-dark_{}_{}",
        color_token(&pair.light),
        color_token(&pair.dark)
    );
    if !token.ends_with('_') {
        token.push('_');
    }
    token.push_str("-1");
    token
}

fn color_token(value: &str) -> String {
    let value = value.trim();
    if let Some(hex) = normalize_hex_color(value) {
        return hex.trim_start_matches('#').to_lowercase();
    }
    if let Some((r, g, b)) = parse_rgb_color(value) {
        return format!("{:02x}{:02x}{:02x}", r, g, b);
    }
    if let Some(inner) = value.strip_prefix("var(").and_then(|v| v.strip_suffix(')')) {
        let cleaned = inner
            .replace(", ", "_")
            .replace([',', '(', ')', ' '], "_")
            .replace('#', "");
        return format!("var_{}_", cleaned);
    }
    value.replace('#', "").to_lowercase()
}

fn normalize_hex_color(value: &str) -> Option<String> {
    let hex = value.strip_prefix('#')?;
    if hex.len() != 6 {
        return None;
    }
    Some(format!("#{}", hex.to_lowercase()))
}

fn normalize_color_value(value: &str) -> String {
    normalize_hex_color(value).unwrap_or_else(|| value.to_string())
}

fn parse_rgb_color(value: &str) -> Option<(u8, u8, u8)> {
    let inner = value.strip_prefix("rgb(")?.strip_suffix(')')?;
    let mut parts = inner.split(',').map(|part| part.trim());
    let r = parts.next()?.parse::<u8>().ok()?;
    let g = parts.next()?.parse::<u8>().ok()?;
    let b = parts.next()?.parse::<u8>().ok()?;
    Some((r, g, b))
}

fn to_rgb_string(value: &str) -> Option<String> {
    if value.trim().starts_with("rgb(") {
        return Some(value.trim().to_string());
    }
    let hex = normalize_hex_color(value)?;
    let bytes = u32::from_str_radix(hex.trim_start_matches('#'), 16).ok()?;
    let r = ((bytes >> 16) & 0xFF) as u8;
    let g = ((bytes >> 8) & 0xFF) as u8;
    let b = (bytes & 0xFF) as u8;
    Some(format!("rgb({}, {}, {})", r, g, b))
}

fn gradient_direction_coords(
    direction: &str,
) -> (&'static str, &'static str, &'static str, &'static str) {
    match direction {
        "north" => ("0%", "100%", "0%", "0%"),
        "east" => ("0%", "0%", "100%", "0%"),
        "west" => ("100%", "0%", "0%", "0%"),
        _ => ("0%", "0%", "0%", "100%"),
    }
}

fn unescape_html(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '&' {
            let mut entity = String::new();
            while let Some(&next) = chars.peek() {
                chars.next();
                if next == ';' {
                    break;
                }
                entity.push(next);
            }
            match entity.as_str() {
                "lt" => out.push('<'),
                "gt" => out.push('>'),
                "amp" => out.push('&'),
                "quot" => out.push('"'),
                "#39" => out.push('\''),
                "nbsp" => out.push(' '),
                _ => {
                    out.push('&');
                    out.push_str(&entity);
                    out.push(';');
                }
            }
        } else {
            out.push(ch);
        }
    }
    out
}

fn style_value<'a>(style: Option<&'a str>, key: &str) -> Option<&'a str> {
    let style = style?;
    style.split(';').find_map(|entry| {
        let (k, v) = entry.split_once('=')?;
        if k == key { Some(v) } else { None }
    })
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
