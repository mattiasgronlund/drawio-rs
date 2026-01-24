use crate::model::{Diagram, MxCell, MxFile, MxGeometry, MxGraphModel, MxPoint};
#[cfg(feature = "edge-debug")]
use serde::Serialize;
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

const ROOT_KEY: &str = "__root__";

pub fn generate_svg(mxfile: &MxFile, diagram_index: usize) -> SvgResult<String> {
    let diagram = mxfile
        .diagrams
        .get(diagram_index)
        .ok_or(SvgError::MissingDiagram(diagram_index))?;
    generate_svg_for_diagram(diagram)
}

#[cfg(feature = "edge-debug")]
pub fn debug_render_tree(mxfile: &MxFile, diagram_index: usize) -> SvgResult<String> {
    let diagram = mxfile
        .diagrams
        .get(diagram_index)
        .ok_or(SvgError::MissingDiagram(diagram_index))?;
    let graph = diagram
        .graph_model
        .as_ref()
        .ok_or(SvgError::MissingGraphModel)?;
    let mut children_by_parent: BTreeMap<String, Vec<&MxCell>> = BTreeMap::new();
    for cell in ordered_cells(graph) {
        let parent_key = cell.parent.clone().unwrap_or_else(|| ROOT_KEY.to_string());
        children_by_parent.entry(parent_key).or_default().push(cell);
    }
    let mut visible_by_id: BTreeMap<String, bool> = BTreeMap::new();
    mark_visibility(ROOT_KEY, true, &children_by_parent, &mut visible_by_id);

    let ctx = RenderTreeCtx {
        children_by_parent: &children_by_parent,
        visible_by_id: &visible_by_id,
    };
    let mut out = String::new();
    render_tree(ROOT_KEY, 0, &ctx, &mut out);
    Ok(out)
}

#[cfg(feature = "edge-debug")]
pub fn debug_graph_bounds(mxfile: &MxFile, diagram_index: usize) -> SvgResult<DebugGraphBounds> {
    let diagram = mxfile
        .diagrams
        .get(diagram_index)
        .ok_or(SvgError::MissingDiagram(diagram_index))?;
    let graph = diagram
        .graph_model
        .as_ref()
        .ok_or(SvgError::MissingGraphModel)?;

    let mut cell_by_id = BTreeMap::new();
    let mut children_by_parent: BTreeMap<String, Vec<&MxCell>> = BTreeMap::new();
    for cell in ordered_cells(graph) {
        let parent_key = cell.parent.clone().unwrap_or_else(|| ROOT_KEY.to_string());
        children_by_parent.entry(parent_key).or_default().push(cell);
        cell_by_id.insert(cell.id.clone(), cell);
    }
    let mut visible_by_id: BTreeMap<String, bool> = BTreeMap::new();
    mark_visibility(ROOT_KEY, true, &children_by_parent, &mut visible_by_id);

    let mut vertices = Vec::new();
    let mut edges = Vec::new();
    for cell in ordered_cells(graph) {
        if !visible_by_id.get(&cell.id).copied().unwrap_or(true) {
            continue;
        }
        if cell.edge == Some(true) {
            edges.push(cell);
        } else if cell.vertex == Some(true) {
            if is_edge_label(cell.style.as_deref()) {
                continue;
            }
            if is_group_cell(cell) {
                continue;
            }
            vertices.push(cell);
        }
    }

    debug_bounds_for_graph(&vertices, &edges, &cell_by_id)
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
    let mut cell_by_id = BTreeMap::new();
    let mut children_by_parent: BTreeMap<String, Vec<&MxCell>> = BTreeMap::new();
    for cell in ordered_cells(graph) {
        cell_by_id.insert(cell.id.clone(), cell);
        let parent_key = cell.parent.clone().unwrap_or_else(|| ROOT_KEY.to_string());
        children_by_parent.entry(parent_key).or_default().push(cell);
    }

    let mut visible_by_id: BTreeMap<String, bool> = BTreeMap::new();
    mark_visibility(ROOT_KEY, true, &children_by_parent, &mut visible_by_id);

    let mut vertices = Vec::new();
    let mut edges = Vec::new();
    let mut edge_labels: BTreeMap<String, Vec<&MxCell>> = BTreeMap::new();
    for cell in ordered_cells(graph) {
        if !visible_by_id.get(&cell.id).copied().unwrap_or(true) {
            continue;
        }
        if cell.edge == Some(true) {
            edges.push(cell);
        } else if cell.vertex == Some(true) {
            if is_edge_label(cell.style.as_deref()) {
                if let Some(parent) = cell.parent.as_ref()
                    && visible_by_id.get(parent).copied().unwrap_or(true)
                {
                    edge_labels.entry(parent.clone()).or_default().push(cell);
                }
                continue;
            }
            if is_group_cell(cell) {
                continue;
            }
            vertices.push(cell);
        }
    }

    let (min_x, min_y, width, height) = bounds_for_graph(&vertices, &edges, &cell_by_id)?;
    let min_x = snap_coord(min_x);
    let min_y = snap_coord(min_y);
    let mut out = String::new();
    let (svg_width, svg_height, view_width, view_height) =
        if vertices.is_empty() && edges.is_empty() {
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
    out.push_str("<g>");
    render_cells_recursive(
        ROOT_KEY,
        &mut out,
        &children_by_parent,
        &visible_by_id,
        &cell_by_id,
        &edge_labels,
        &gradient_fill_by_id,
        min_x,
        min_y,
        &mut requires_extensibility,
    )?;
    out.push_str("</g>");
    if requires_extensibility {
        out.push_str(WARNING_SWITCH);
    }
    out.push_str("</svg>");
    Ok(out)
}

fn ordered_cells(graph: &MxGraphModel) -> Vec<&MxCell> {
    let mut cells: Vec<&MxCell> = graph
        .root
        .cells
        .iter()
        .chain(graph.user_objects.iter())
        .collect();
    cells.sort_by_key(|cell| cell.order);
    cells
}

#[cfg(feature = "edge-debug")]
struct RenderTreeCtx<'a> {
    children_by_parent: &'a BTreeMap<String, Vec<&'a MxCell>>,
    visible_by_id: &'a BTreeMap<String, bool>,
}

#[cfg(feature = "edge-debug")]
fn render_tree(parent_key: &str, depth: usize, ctx: &RenderTreeCtx<'_>, out: &mut String) {
    let Some(children) = ctx.children_by_parent.get(parent_key) else {
        return;
    };
    for cell in children {
        let visible = ctx.visible_by_id.get(&cell.id).copied().unwrap_or(true);
        let kind = cell_kind(cell);
        let value = cell.value.as_deref().unwrap_or("");
        let indent = "  ".repeat(depth);
        let _ = writeln!(
            out,
            "{indent}- id={} kind={} visible={} value=\"{}\"",
            cell.id, kind, visible, value
        );
        render_tree(cell.id.as_str(), depth + 1, ctx, out);
    }
}

#[cfg(feature = "edge-debug")]
fn cell_kind(cell: &MxCell) -> &'static str {
    if is_group_cell(cell) {
        return "group";
    }
    if cell.edge == Some(true) {
        return "edge";
    }
    if cell.vertex == Some(true) {
        return "vertex";
    }
    "layer"
}

#[allow(clippy::too_many_arguments)]
fn render_cells_recursive(
    parent_key: &str,
    out: &mut String,
    children_by_parent: &BTreeMap<String, Vec<&MxCell>>,
    visible_by_id: &BTreeMap<String, bool>,
    cell_by_id: &BTreeMap<String, &MxCell>,
    edge_labels: &BTreeMap<String, Vec<&MxCell>>,
    gradient_fill_by_id: &BTreeMap<String, String>,
    min_x: f64,
    min_y: f64,
    requires_extensibility: &mut bool,
) -> SvgResult<()> {
    let Some(children) = children_by_parent.get(parent_key) else {
        return Ok(());
    };
    for cell in children {
        if !visible_by_id.get(&cell.id).copied().unwrap_or(true) {
            continue;
        }
        if cell.edge == Some(true) {
            let edge_label_cells = edge_labels.get(&cell.id).map(Vec::as_slice).unwrap_or(&[]);
            if let Some(edge_render) =
                render_edge(cell, cell_by_id, min_x, min_y, edge_label_cells)?
            {
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
                for label in edge_render.labels {
                    out.push_str(&label);
                    *requires_extensibility = true;
                }
                out.push_str("</g>");
            }
            continue;
        }
        if cell.vertex == Some(true) {
            if is_edge_label(cell.style.as_deref()) {
                continue;
            }
            if is_group_cell(cell) {
                let transform = vertex_transform(cell.style.as_deref());
                match transform {
                    Some(value) => {
                        write!(
                            out,
                            "<g data-cell-id=\"{}\"><g transform=\"{}\"></g>",
                            cell.id, value
                        )
                        .unwrap();
                    }
                    None => {
                        write!(out, "<g data-cell-id=\"{}\"><g></g>", cell.id).unwrap();
                    }
                }
                render_cells_recursive(
                    cell.id.as_str(),
                    out,
                    children_by_parent,
                    visible_by_id,
                    cell_by_id,
                    edge_labels,
                    gradient_fill_by_id,
                    min_x,
                    min_y,
                    requires_extensibility,
                )?;
                out.push_str("</g>");
                continue;
            }
            let geometry = cell
                .geometry
                .as_ref()
                .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
            let width = geometry.width.unwrap_or(0.0);
            let height = geometry.height.unwrap_or(0.0);
            let top_left = vertex_top_left(cell, cell_by_id)?;
            let raw_top_left = vertex_top_left_raw(cell, cell_by_id)?;
            let x = top_left.x - min_x;
            let y = top_left.y - min_y;
            let raw_x = raw_top_left.x - min_x;
            let raw_y = raw_top_left.y - min_y;
            let style = cell.style.as_deref();
            let dash_attr = if is_dashed(style) {
                " stroke-dasharray=\"3 3\""
            } else {
                ""
            };
            let stroke_width_attr = stroke_width_attr(style);
            let gradient_fill = gradient_fill_by_id.get(&cell.id).map(|id| id.as_str());
            let (fill_attr, stroke_attr, style_attr) = shape_paint_attrs(style, gradient_fill);
            let (_, stroke_style_attr) = edge_stroke_attrs(style);
            let rotation_attr = shape_rotation_attr(style, x, y, width, height);
            let shape = style_value(style, "shape");
            let image_value = if shape == Some("image") {
                style_value(style, "image")
            } else {
                None
            };
            let is_image = image_value.is_some();
            let transform = vertex_transform(style);
            let (open, close) = if is_image {
                ("<g>".to_string(), "</g>".to_string())
            } else {
                match transform {
                    Some(value) => (format!("<g transform=\"{}\">", value), "</g>".to_string()),
                    None => ("<g>".to_string(), "</g>".to_string()),
                }
            };
            if is_swimlane(style) {
                let start_size = swimlane_start_size(style);
                let vertical_swimlane = swimlane_is_vertical(cell);
                if vertical_swimlane {
                    let header_x = x + start_size;
                    write!(
                        out,
                        "<g data-cell-id=\"{}\">{}<path d=\"M {} {} L {} {} L {} {} L {} {}\" fill=\"{}\" pointer-events=\"all\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} /><path d=\"M {} {} L {} {} L {} {} L {} {}\" fill=\"none\" pointer-events=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} /><path d=\"M {} {} L {} {}\" fill=\"none\" pointer-events=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} />{}",
                        cell.id,
                        open,
                        fmt_num(header_x),
                        fmt_num(y),
                        fmt_num(x),
                        fmt_num(y),
                        fmt_num(x),
                        fmt_num(y + height),
                        fmt_num(header_x),
                        fmt_num(y + height),
                        fill_attr,
                        stroke_attr,
                        stroke_width_attr,
                        style_attr,
                        rotation_attr,
                        fmt_num(header_x),
                        fmt_num(y),
                        fmt_num(x + width),
                        fmt_num(y),
                        fmt_num(x + width),
                        fmt_num(y + height),
                        fmt_num(header_x),
                        fmt_num(y + height),
                        stroke_attr,
                        stroke_width_attr,
                        stroke_style_attr,
                        rotation_attr,
                        fmt_num(header_x),
                        fmt_num(y),
                        fmt_num(header_x),
                        fmt_num(y + height),
                        stroke_attr,
                        stroke_width_attr,
                        stroke_style_attr,
                        rotation_attr,
                        close
                    )
                    .unwrap();
                } else {
                    let header_y = y + start_size;
                    write!(
                        out,
                        "<g data-cell-id=\"{}\">{}<path d=\"M {} {} L {} {} L {} {} L {} {}\" fill=\"{}\" pointer-events=\"all\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} /><path d=\"M {} {} L {} {} L {} {} L {} {}\" fill=\"none\" pointer-events=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} /><path d=\"M {} {} L {} {}\" fill=\"none\" pointer-events=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} />{}",
                        cell.id,
                        open,
                        fmt_num(x),
                        fmt_num(header_y),
                        fmt_num(x),
                        fmt_num(y),
                        fmt_num(x + width),
                        fmt_num(y),
                        fmt_num(x + width),
                        fmt_num(header_y),
                        fill_attr,
                        stroke_attr,
                        stroke_width_attr,
                        style_attr,
                        rotation_attr,
                        fmt_num(x),
                        fmt_num(header_y),
                        fmt_num(x),
                        fmt_num(y + height),
                        fmt_num(x + width),
                        fmt_num(y + height),
                        fmt_num(x + width),
                        fmt_num(header_y),
                        stroke_attr,
                        stroke_width_attr,
                        stroke_style_attr,
                        rotation_attr,
                        fmt_num(x),
                        fmt_num(header_y),
                        fmt_num(x + width),
                        fmt_num(header_y),
                        stroke_attr,
                        stroke_width_attr,
                        stroke_style_attr,
                        rotation_attr,
                        close
                    )
                    .unwrap();
                }
            } else if is_partial_rectangle(style) {
                let top = style_value(style, "top") != Some("0");
                let right = style_value(style, "right") != Some("0");
                let bottom = style_value(style, "bottom") != Some("0");
                let left = style_value(style, "left") != Some("0");
                let mut d = String::new();
                write!(d, "M {} {}", fmt_num(x), fmt_num(y)).unwrap();
                if top {
                    write!(d, " L {} {}", fmt_num(x + width), fmt_num(y)).unwrap();
                } else {
                    write!(d, " M {} {}", fmt_num(x + width), fmt_num(y)).unwrap();
                }
                if right {
                    write!(d, " L {} {}", fmt_num(x + width), fmt_num(y + height)).unwrap();
                } else {
                    write!(d, " M {} {}", fmt_num(x + width), fmt_num(y + height)).unwrap();
                }
                if bottom {
                    write!(d, " L {} {}", fmt_num(x), fmt_num(y + height)).unwrap();
                } else {
                    write!(d, " M {} {}", fmt_num(x), fmt_num(y + height)).unwrap();
                }
                if left {
                    write!(d, " L {} {}", fmt_num(x), fmt_num(y)).unwrap();
                }
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<rect fill=\"none\" height=\"{}\" pointer-events=\"all\" stroke=\"none\" width=\"{}\" x=\"{}\" y=\"{}\"{} /><path d=\"{}\" fill=\"none\" pointer-events=\"all\" stroke=\"{}\" stroke-linecap=\"square\" stroke-miterlimit=\"10\"{}{}{} />{}",
                    cell.id,
                    open,
                    fmt_num(height),
                    fmt_num(width),
                    fmt_num(x),
                    fmt_num(y),
                    rotation_attr,
                    d,
                    stroke_attr,
                    stroke_width_attr,
                    stroke_style_attr,
                    rotation_attr,
                    close
                )
                .unwrap();
            } else if style_value(style, "shape") == Some("message") {
                let (edge_stroke, edge_style_attr) = edge_stroke_attrs(style);
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<path d=\"M {} {} L {} {} L {} {} L {} {} Z\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{}{}{} /><path d=\"M {} {} L {} {} L {} {}\" fill=\"none\" stroke=\"{}\" pointer-events=\"all\"{}{}{}{} />{}",
                    cell.id,
                    open,
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(x + width),
                    fmt_num(y),
                    fmt_num(x + width),
                    fmt_num(y + height),
                    fmt_num(x),
                    fmt_num(y + height),
                    fill_attr,
                    stroke_attr,
                    dash_attr,
                    stroke_width_attr,
                    style_attr,
                    rotation_attr,
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(x + width / 2.0),
                    fmt_num(y + height / 2.0),
                    fmt_num(x + width),
                    fmt_num(y),
                    edge_stroke,
                    dash_attr,
                    stroke_width_attr,
                    edge_style_attr,
                    rotation_attr,
                    close
                )
                .unwrap();
            } else if is_uml_actor(style) {
                let head_radius = width / 4.0;
                let center_x = x + width / 2.0;
                let head_center_y = y + head_radius;
                let body_top = y + head_radius * 2.0;
                let body_bottom = y + height * (2.0 / 3.0);
                let arms_y = y + height / 3.0;
                let leg_y = y + height;
                let arm_span = width / 2.0;
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{}{}{} /><path d=\"M {} {} L {} {} M {} {} L {} {} M {} {} L {} {} M {} {} L {} {} M {} {} L {} {}\" fill=\"none\" pointer-events=\"all\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} />{}",
                    cell.id,
                    open,
                    fmt_num(center_x),
                    fmt_num(head_center_y),
                    fmt_num(head_radius),
                    fmt_num(head_radius),
                    fill_attr,
                    stroke_attr,
                    dash_attr,
                    stroke_width_attr,
                    style_attr,
                    rotation_attr,
                    fmt_num(center_x),
                    fmt_num(body_top),
                    fmt_num(center_x),
                    fmt_num(body_bottom),
                    fmt_num(center_x),
                    fmt_num(arms_y),
                    fmt_num(center_x - arm_span),
                    fmt_num(arms_y),
                    fmt_num(center_x),
                    fmt_num(arms_y),
                    fmt_num(center_x + arm_span),
                    fmt_num(arms_y),
                    fmt_num(center_x),
                    fmt_num(body_bottom),
                    fmt_num(center_x - arm_span),
                    fmt_num(leg_y),
                    fmt_num(center_x),
                    fmt_num(body_bottom),
                    fmt_num(center_x + arm_span),
                    fmt_num(leg_y),
                    stroke_attr,
                    stroke_width_attr,
                    stroke_style_attr,
                    rotation_attr,
                    close
                )
                .unwrap();
            } else if is_image {
                let href = normalize_image_href(
                    image_value.expect("image_value is Some when is_image is true"),
                );
                let preserve = if style_value(style, "imageAspect") == Some("0") {
                    "none"
                } else {
                    "xMidYMid meet"
                };
                let rotation_attr = image_rotation_attr(style, x, y, width, height);
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<image x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" xlink:href=\"{}\" preserveAspectRatio=\"{}\"{}></image>{}",
                    cell.id,
                    open,
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(width),
                    fmt_num(height),
                    href,
                    preserve,
                    rotation_attr,
                    close
                )
                .unwrap();
            } else if style_value(style, "shape") == Some("datastore") {
                let dy = (height / 7.5).min(height / 2.0);
                let top_y = y + dy;
                let bottom_y = y + height - dy;
                let control_top = y - dy / 3.0;
                let control_bottom = y + height + dy / 3.0;
                let mid_y = top_y + dy / 2.0;
                let line_low = top_y + dy;
                let control_mid = mid_y + dy;
                let control_low = line_low + dy;
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<path d=\"M {} {} C {} {} {} {} {} {} L {} {} C {} {} {} {} {} {} Z\" fill=\"{}\" pointer-events=\"all\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} /><path d=\"M {} {} C {} {} {} {} {} {} M {} {} C {} {} {} {} {} {} M {} {} C {} {} {} {} {} {}\" fill=\"none\" pointer-events=\"all\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} />{}",
                    cell.id,
                    open,
                    fmt_num(x),
                    fmt_num(top_y),
                    fmt_num(x),
                    fmt_num(control_top),
                    fmt_num(x + width),
                    fmt_num(control_top),
                    fmt_num(x + width),
                    fmt_num(top_y),
                    fmt_num(x + width),
                    fmt_num(bottom_y),
                    fmt_num(x + width),
                    fmt_num(control_bottom),
                    fmt_num(x),
                    fmt_num(control_bottom),
                    fmt_num(x),
                    fmt_num(bottom_y),
                    fill_attr,
                    stroke_attr,
                    stroke_width_attr,
                    style_attr,
                    rotation_attr,
                    fmt_num(x),
                    fmt_num(top_y),
                    fmt_num(x),
                    fmt_num(line_low),
                    fmt_num(x + width),
                    fmt_num(line_low),
                    fmt_num(x + width),
                    fmt_num(top_y),
                    fmt_num(x),
                    fmt_num(mid_y),
                    fmt_num(x),
                    fmt_num(control_mid),
                    fmt_num(x + width),
                    fmt_num(control_mid),
                    fmt_num(x + width),
                    fmt_num(mid_y),
                    fmt_num(x),
                    fmt_num(line_low),
                    fmt_num(x),
                    fmt_num(control_low),
                    fmt_num(x + width),
                    fmt_num(control_low),
                    fmt_num(x + width),
                    fmt_num(line_low),
                    stroke_attr,
                    stroke_width_attr,
                    stroke_style_attr,
                    rotation_attr,
                    close
                )
                .unwrap();
            } else if style_value(style, "shape") == Some("switch") {
                let center_x = x + width / 2.0;
                let center_y = y + height / 2.0;
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<path d=\"M {} {} Q {} {} {} {} Q {} {} {} {} Q {} {} {} {} Q {} {} {} {}\" fill=\"{}\" pointer-events=\"all\" stroke=\"{}\" stroke-miterlimit=\"10\"{}{}{} />{}",
                    cell.id,
                    open,
                    fmt_num(x),
                    fmt_num(y),
                    fmt_num(center_x),
                    fmt_num(center_y),
                    fmt_num(x + width),
                    fmt_num(y),
                    fmt_num(center_x),
                    fmt_num(center_y),
                    fmt_num(x + width),
                    fmt_num(y + height),
                    fmt_num(center_x),
                    fmt_num(center_y),
                    fmt_num(x),
                    fmt_num(y + height),
                    fmt_num(center_x),
                    fmt_num(center_y),
                    fmt_num(x),
                    fmt_num(y),
                    fill_attr,
                    stroke_attr,
                    stroke_width_attr,
                    style_attr,
                    rotation_attr,
                    close
                )
                .unwrap();
            } else if is_ellipse(style) {
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{}{}{} />{}",
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
                    rotation_attr,
                    close
                )
                .unwrap();
            } else {
                let rounding = if is_rounded(style) {
                    let radius = style_value(style, "arcSize")
                        .and_then(|value| value.parse::<f64>().ok())
                        .map(|arc| width.min(height) * arc / 100.0)
                        .unwrap_or_else(|| (width.min(height) * 0.15).clamp(3.0, 10.0));
                    format!(" rx=\"{}\" ry=\"{}\"", fmt_num(radius), fmt_num(radius))
                } else {
                    String::new()
                };
                write!(
                    out,
                    "<g data-cell-id=\"{}\">{}<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{}{}{}{} />{}",
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
                    rotation_attr,
                    close
                )
                .unwrap();
            }
            if is_swimlane(style) {
                let vertical_swimlane = swimlane_is_vertical(cell);
                if let Some((label, uses_ext)) = render_swimlane_label(
                    cell,
                    x,
                    y,
                    raw_x,
                    raw_y,
                    width,
                    height,
                    swimlane_start_size(style),
                    vertical_swimlane,
                ) {
                    out.push_str(&label);
                    if uses_ext {
                        *requires_extensibility = true;
                    }
                }
            } else if let Some(label) = render_vertex_label(cell, x, y, width, height) {
                out.push_str(&label);
                *requires_extensibility = true;
            }
            if children_by_parent.contains_key(cell.id.as_str()) && !is_collapsed(cell) {
                render_cells_recursive(
                    cell.id.as_str(),
                    out,
                    children_by_parent,
                    visible_by_id,
                    cell_by_id,
                    edge_labels,
                    gradient_fill_by_id,
                    min_x,
                    min_y,
                    requires_extensibility,
                )?;
            }
            out.push_str("</g>");
            continue;
        }
        write!(out, "<g data-cell-id=\"{}\">", cell.id).unwrap();
        render_cells_recursive(
            cell.id.as_str(),
            out,
            children_by_parent,
            visible_by_id,
            cell_by_id,
            edge_labels,
            gradient_fill_by_id,
            min_x,
            min_y,
            requires_extensibility,
        )?;
        out.push_str("</g>");
    }
    Ok(())
}

fn bounds_for_graph(
    vertices: &[&MxCell],
    edges: &[&MxCell],
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<(f64, f64, f64, f64)> {
    if vertices.is_empty() && edges.is_empty() {
        return Ok((0.0, 0.0, 0.0, 0.0));
    }
    let has_image = vertices.iter().any(|vertex| {
        let style = vertex.style.as_deref();
        style_value(style, "shape") == Some("image") && style_value(style, "image").is_some()
    });

    let mut min_x = f64::MAX;
    let mut min_y = f64::MAX;
    let mut max_x = f64::MIN;
    let mut max_y = f64::MIN;

    for vertex in vertices {
        let geometry = vertex
            .geometry
            .as_ref()
            .ok_or_else(|| SvgError::MissingGeometry(vertex.id.clone()))?;
        let top_left = vertex_top_left(vertex, cell_by_id)?;
        let x = top_left.x;
        let y = top_left.y;
        let width = geometry.width.unwrap_or(0.0);
        let height = geometry.height.unwrap_or(0.0);
        let style = vertex.style.as_deref();
        let stroke_width = stroke_width_value(style);
        let half = (stroke_width / 2.0).floor();
        let rotation = rotation_degrees(style);
        let is_image =
            style_value(style, "shape") == Some("image") && style_value(style, "image").is_some();
        let mut bbox = if rotation.abs() > f64::EPSILON {
            rotated_bbox(x, y, width, height, rotation)
        } else {
            BBox {
                min_x: x,
                min_y: y,
                max_x: x + width,
                max_y: y + height,
            }
        };
        if is_image {
            bbox.min_x = bbox.min_x.ceil();
            bbox.min_y = bbox.min_y.ceil();
        } else if has_image && vertex_transform(style).is_some() {
            bbox.min_x += 0.5;
            bbox.min_y += 0.5;
            bbox.max_x += 0.5;
            bbox.max_y += 0.5;
        }
        min_x = min_x.min(bbox.min_x - half);
        min_y = min_y.min(bbox.min_y - half);
        max_x = max_x.max(bbox.max_x + half);
        max_y = max_y.max(bbox.max_y + half);
    }

    for edge in edges {
        if let Some(edge_path) = edge_path_absolute(edge, cell_by_id)? {
            for point in edge_path
                .full_points
                .iter()
                .chain(edge_path.arrow_points.iter())
            {
                min_x = min_x.min(point.x);
                min_y = min_y.min(point.y);
                max_x = max_x.max(point.x);
                max_y = max_y.max(point.y);
            }
            let style = edge.style.as_deref();
            let shape = style_value(style, "shape");
            if shape == Some("flexArrow") {
                let metrics = flex_arrow_metrics();
                let centerline = flex_arrow_centerline(edge, &edge_path);
                for point in &centerline {
                    min_x = min_x.min(point.x - metrics.head_half);
                    min_y = min_y.min(point.y - metrics.head_half);
                    max_x = max_x.max(point.x + metrics.head_half);
                    max_y = max_y.max(point.y + metrics.head_half);
                }
                let start_head = match style_value(style, "startArrow") {
                    Some("none") | Some("0") => false,
                    Some(_) => true,
                    None => false,
                };
                let end_head = !matches!(style_value(style, "endArrow"), Some("none") | Some("0"));
                if end_head
                    && let Some((tip, left, right)) =
                        flex_arrow_head_points(&centerline, true, metrics)
                {
                    for point in [tip, left, right] {
                        min_x = min_x.min(point.x);
                        min_y = min_y.min(point.y);
                        max_x = max_x.max(point.x);
                        max_y = max_y.max(point.y);
                    }
                }
                if start_head
                    && let Some((tip, left, right)) =
                        flex_arrow_head_points(&centerline, false, metrics)
                {
                    for point in [tip, left, right] {
                        min_x = min_x.min(point.x);
                        min_y = min_y.min(point.y);
                        max_x = max_x.max(point.x);
                        max_y = max_y.max(point.y);
                    }
                }
            }
            if !matches!(shape, Some("arrow" | "link" | "flexArrow")) {
                let stroke_width = stroke_width_value(style);
                let metrics = edge_arrow_metrics(stroke_width);
                let start_kind = marker_kind_from_value(style_value(style, "startArrow"));
                let end_kind = match style_value(style, "endArrow") {
                    Some("none") => None,
                    Some(value) => marker_kind_from_value(Some(value)),
                    None => Some(MarkerKind::Classic),
                };
                if let Some(kind) = start_kind {
                    expand_marker_bounds(
                        &mut min_x,
                        &mut min_y,
                        &mut max_x,
                        &mut max_y,
                        edge_path.source_anchor,
                        edge_path.start_dir,
                        kind,
                        stroke_width,
                        metrics,
                    );
                }
                if let Some(kind) = end_kind {
                    expand_marker_bounds(
                        &mut min_x,
                        &mut min_y,
                        &mut max_x,
                        &mut max_y,
                        edge_path.target_anchor,
                        Point {
                            x: -edge_path.end_dir.x,
                            y: -edge_path.end_dir.y,
                        },
                        kind,
                        stroke_width,
                        metrics,
                    );
                }
            }
        }
    }

    if min_x == f64::MAX || min_y == f64::MAX {
        return Ok((0.0, 0.0, 0.0, 0.0));
    }

    let mut extra_width: f64 = 0.0;
    let mut extra_height: f64 = 0.0;
    for edge in edges {
        if edge.source.is_none() && edge.target.is_none() {
            let (tip_gap, arrow_length, arrow_back, _arrow_half_height) =
                edge_arrow_metrics(stroke_width_value(edge.style.as_deref()));
            extra_width = extra_width.max(tip_gap + arrow_length + arrow_back);
            extra_height = extra_height.max(tip_gap);
        }
    }

    if extra_width > 0.0 || extra_height > 0.0 {
        let padding = extra_height;
        min_x -= padding;
        min_y -= padding;
        extra_width = (extra_width - padding).max(0.0);
    }

    let raw_min_x = min_x;
    let raw_min_y = min_y;
    let min_x = min_x.floor();
    let min_y = min_y.floor();
    if extra_width > 0.0 {
        extra_width = (extra_width - (raw_min_x - min_x)).max(0.0);
    }
    if extra_height > 0.0 {
        extra_height = (extra_height - (raw_min_y - min_y)).max(0.0);
    }
    let width = (max_x - min_x + extra_width).round();
    let height = (max_y - min_y + extra_height).round();
    Ok((min_x, min_y, width, height))
}

#[cfg(feature = "edge-debug")]
fn debug_bounds_for_graph(
    vertices: &[&MxCell],
    edges: &[&MxCell],
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<DebugGraphBounds> {
    if vertices.is_empty() && edges.is_empty() {
        return Ok(DebugGraphBounds {
            raw_min_x: 0.0,
            raw_min_y: 0.0,
            raw_max_x: 0.0,
            raw_max_y: 0.0,
            min_x: 0.0,
            min_y: 0.0,
            width: 0.0,
            height: 0.0,
            extra_width: 0.0,
            extra_height: 0.0,
            vertices: Vec::new(),
            edges: Vec::new(),
        });
    }

    let has_image = vertices.iter().any(|vertex| {
        let style = vertex.style.as_deref();
        style_value(style, "shape") == Some("image") && style_value(style, "image").is_some()
    });

    let update_bounds =
        |min_x: &mut f64, min_y: &mut f64, max_x: &mut f64, max_y: &mut f64, point: Point| {
            *min_x = min_x.min(point.x);
            *min_y = min_y.min(point.y);
            *max_x = max_x.max(point.x);
            *max_y = max_y.max(point.y);
        };

    let make_bbox = |min_x: f64, min_y: f64, max_x: f64, max_y: f64| DebugBBox {
        min_x,
        min_y,
        max_x,
        max_y,
    };

    let mut min_x = f64::MAX;
    let mut min_y = f64::MAX;
    let mut max_x = f64::MIN;
    let mut max_y = f64::MIN;

    let mut vertex_bounds = Vec::new();
    for vertex in vertices {
        let geometry = vertex
            .geometry
            .as_ref()
            .ok_or_else(|| SvgError::MissingGeometry(vertex.id.clone()))?;
        let top_left = vertex_top_left(vertex, cell_by_id)?;
        let x = top_left.x;
        let y = top_left.y;
        let width = geometry.width.unwrap_or(0.0);
        let height = geometry.height.unwrap_or(0.0);
        let style = vertex.style.as_deref();
        let stroke_width = stroke_width_value(style);
        let half = (stroke_width / 2.0).floor();
        let rotation = rotation_degrees(style);
        let is_image =
            style_value(style, "shape") == Some("image") && style_value(style, "image").is_some();
        let has_transform = vertex_transform(style).is_some();
        let mut bbox = if rotation.abs() > f64::EPSILON {
            rotated_bbox(x, y, width, height, rotation)
        } else {
            BBox {
                min_x: x,
                min_y: y,
                max_x: x + width,
                max_y: y + height,
            }
        };
        if is_image {
            bbox.min_x = bbox.min_x.ceil();
            bbox.min_y = bbox.min_y.ceil();
        } else if has_image && has_transform {
            bbox.min_x += 0.5;
            bbox.min_y += 0.5;
            bbox.max_x += 0.5;
            bbox.max_y += 0.5;
        }
        let bbox_before_stroke = make_bbox(bbox.min_x, bbox.min_y, bbox.max_x, bbox.max_y);
        let bbox_after_stroke = make_bbox(
            bbox.min_x - half,
            bbox.min_y - half,
            bbox.max_x + half,
            bbox.max_y + half,
        );
        min_x = min_x.min(bbox_after_stroke.min_x);
        min_y = min_y.min(bbox_after_stroke.min_y);
        max_x = max_x.max(bbox_after_stroke.max_x);
        max_y = max_y.max(bbox_after_stroke.max_y);
        vertex_bounds.push(DebugVertexBounds {
            id: vertex.id.clone(),
            x,
            y,
            width,
            height,
            rotation,
            stroke_width,
            is_image,
            has_transform,
            bbox_before_stroke,
            bbox_after_stroke,
        });
    }

    let mut edge_bounds = Vec::new();
    let mut extra_width: f64 = 0.0;
    let mut extra_height: f64 = 0.0;
    for edge in edges {
        let Some(edge_path) = edge_path_absolute(edge, cell_by_id)? else {
            continue;
        };
        let style = edge.style.as_deref();
        let shape = style_value(style, "shape").map(|value| value.to_string());
        let edge_style = style_value(style, "edgeStyle").map(|value| value.to_string());
        let start_arrow = style_value(style, "startArrow").map(|value| value.to_string());
        let end_arrow = style_value(style, "endArrow").map(|value| value.to_string());
        let stroke_width = stroke_width_value(style);

        let mut edge_min_x = f64::MAX;
        let mut edge_min_y = f64::MAX;
        let mut edge_max_x = f64::MIN;
        let mut edge_max_y = f64::MIN;
        for point in edge_path
            .full_points
            .iter()
            .chain(edge_path.arrow_points.iter())
        {
            update_bounds(
                &mut edge_min_x,
                &mut edge_min_y,
                &mut edge_max_x,
                &mut edge_max_y,
                *point,
            );
        }
        let base_bbox = if edge_min_x == f64::MAX {
            None
        } else {
            Some(make_bbox(edge_min_x, edge_min_y, edge_max_x, edge_max_y))
        };

        let mut flex_bbox = None;
        if shape.as_deref() == Some("flexArrow") {
            let metrics = flex_arrow_metrics();
            let centerline = flex_arrow_centerline(edge, &edge_path);
            let mut flex_min_x = f64::MAX;
            let mut flex_min_y = f64::MAX;
            let mut flex_max_x = f64::MIN;
            let mut flex_max_y = f64::MIN;
            for point in &centerline {
                update_bounds(
                    &mut flex_min_x,
                    &mut flex_min_y,
                    &mut flex_max_x,
                    &mut flex_max_y,
                    Point {
                        x: point.x - metrics.head_half,
                        y: point.y - metrics.head_half,
                    },
                );
                update_bounds(
                    &mut flex_min_x,
                    &mut flex_min_y,
                    &mut flex_max_x,
                    &mut flex_max_y,
                    Point {
                        x: point.x + metrics.head_half,
                        y: point.y + metrics.head_half,
                    },
                );
            }
            let start_head = match style_value(style, "startArrow") {
                Some("none") | Some("0") => false,
                Some(_) => true,
                None => false,
            };
            let end_head = !matches!(style_value(style, "endArrow"), Some("none") | Some("0"));
            if end_head
                && let Some((tip, left, right)) = flex_arrow_head_points(&centerline, true, metrics)
            {
                for point in [tip, left, right] {
                    update_bounds(
                        &mut flex_min_x,
                        &mut flex_min_y,
                        &mut flex_max_x,
                        &mut flex_max_y,
                        point,
                    );
                }
            }
            if start_head
                && let Some((tip, left, right)) =
                    flex_arrow_head_points(&centerline, false, metrics)
            {
                for point in [tip, left, right] {
                    update_bounds(
                        &mut flex_min_x,
                        &mut flex_min_y,
                        &mut flex_max_x,
                        &mut flex_max_y,
                        point,
                    );
                }
            }
            if flex_min_x != f64::MAX {
                flex_bbox = Some(make_bbox(flex_min_x, flex_min_y, flex_max_x, flex_max_y));
                edge_min_x = edge_min_x.min(flex_min_x);
                edge_min_y = edge_min_y.min(flex_min_y);
                edge_max_x = edge_max_x.max(flex_max_x);
                edge_max_y = edge_max_y.max(flex_max_y);
            }
        }

        let mut marker_start_bbox = None;
        let mut marker_end_bbox = None;
        let mut marker_start_kind = None;
        let mut marker_end_kind = None;
        if !matches!(shape.as_deref(), Some("arrow" | "link" | "flexArrow")) {
            let metrics = edge_arrow_metrics(stroke_width);
            let start_kind = marker_kind_from_value(style_value(style, "startArrow"));
            let end_kind = match style_value(style, "endArrow") {
                Some("none") => None,
                Some(value) => marker_kind_from_value(Some(value)),
                None => Some(MarkerKind::Classic),
            };
            marker_start_kind = start_kind.map(|kind| format!("{:?}", kind));
            marker_end_kind = end_kind.map(|kind| format!("{:?}", kind));
            if let Some(kind) = start_kind {
                let mut mmin_x = f64::MAX;
                let mut mmin_y = f64::MAX;
                let mut mmax_x = f64::MIN;
                let mut mmax_y = f64::MIN;
                expand_marker_bounds(
                    &mut mmin_x,
                    &mut mmin_y,
                    &mut mmax_x,
                    &mut mmax_y,
                    edge_path.source_anchor,
                    edge_path.start_dir,
                    kind,
                    stroke_width,
                    metrics,
                );
                if mmin_x != f64::MAX {
                    marker_start_bbox = Some(make_bbox(mmin_x, mmin_y, mmax_x, mmax_y));
                    edge_min_x = edge_min_x.min(mmin_x);
                    edge_min_y = edge_min_y.min(mmin_y);
                    edge_max_x = edge_max_x.max(mmax_x);
                    edge_max_y = edge_max_y.max(mmax_y);
                }
            }
            if let Some(kind) = end_kind {
                let mut mmin_x = f64::MAX;
                let mut mmin_y = f64::MAX;
                let mut mmax_x = f64::MIN;
                let mut mmax_y = f64::MIN;
                expand_marker_bounds(
                    &mut mmin_x,
                    &mut mmin_y,
                    &mut mmax_x,
                    &mut mmax_y,
                    edge_path.target_anchor,
                    Point {
                        x: -edge_path.end_dir.x,
                        y: -edge_path.end_dir.y,
                    },
                    kind,
                    stroke_width,
                    metrics,
                );
                if mmin_x != f64::MAX {
                    marker_end_bbox = Some(make_bbox(mmin_x, mmin_y, mmax_x, mmax_y));
                    edge_min_x = edge_min_x.min(mmin_x);
                    edge_min_y = edge_min_y.min(mmin_y);
                    edge_max_x = edge_max_x.max(mmax_x);
                    edge_max_y = edge_max_y.max(mmax_y);
                }
            }
        }

        let final_bbox = if edge_min_x == f64::MAX {
            None
        } else {
            Some(make_bbox(edge_min_x, edge_min_y, edge_max_x, edge_max_y))
        };
        if let Some(ref bbox) = final_bbox {
            min_x = min_x.min(bbox.min_x);
            min_y = min_y.min(bbox.min_y);
            max_x = max_x.max(bbox.max_x);
            max_y = max_y.max(bbox.max_y);
        }

        let (edge_extra_width, edge_extra_height) =
            if edge.source.is_none() && edge.target.is_none() {
                let (tip_gap, arrow_length, arrow_back, _arrow_half_height) =
                    edge_arrow_metrics(stroke_width_value(edge.style.as_deref()));
                (tip_gap + arrow_length + arrow_back, tip_gap)
            } else {
                (0.0, 0.0)
            };
        extra_width = extra_width.max(edge_extra_width);
        extra_height = extra_height.max(edge_extra_height);

        edge_bounds.push(DebugEdgeBounds {
            id: edge.id.clone(),
            source_id: edge.source.clone(),
            target_id: edge.target.clone(),
            shape,
            edge_style,
            start_arrow,
            end_arrow,
            stroke_width,
            extra_width: edge_extra_width,
            extra_height: edge_extra_height,
            base_bbox,
            flex_bbox,
            marker_start_bbox,
            marker_end_bbox,
            marker_start_kind,
            marker_end_kind,
            final_bbox,
        });
    }

    if min_x == f64::MAX || min_y == f64::MAX {
        return Ok(DebugGraphBounds {
            raw_min_x: 0.0,
            raw_min_y: 0.0,
            raw_max_x: 0.0,
            raw_max_y: 0.0,
            min_x: 0.0,
            min_y: 0.0,
            width: 0.0,
            height: 0.0,
            extra_width: 0.0,
            extra_height: 0.0,
            vertices: vertex_bounds,
            edges: edge_bounds,
        });
    }

    if extra_width > 0.0 || extra_height > 0.0 {
        let padding = extra_height;
        min_x -= padding;
        min_y -= padding;
        extra_width = (extra_width - padding).max(0.0);
    }

    let raw_min_x = min_x;
    let raw_min_y = min_y;
    let raw_max_x = max_x;
    let raw_max_y = max_y;
    let min_x = min_x.floor();
    let min_y = min_y.floor();
    if extra_width > 0.0 {
        extra_width = (extra_width - (raw_min_x - min_x)).max(0.0);
    }
    if extra_height > 0.0 {
        extra_height = (extra_height - (raw_min_y - min_y)).max(0.0);
    }
    let width = (max_x - min_x + extra_width).round();
    let height = (max_y - min_y + extra_height).round();
    Ok(DebugGraphBounds {
        raw_min_x,
        raw_min_y,
        raw_max_x,
        raw_max_y,
        min_x,
        min_y,
        width,
        height,
        extra_width,
        extra_height,
        vertices: vertex_bounds,
        edges: edge_bounds,
    })
}

#[derive(Clone, Copy, Debug)]
struct Point {
    x: f64,
    y: f64,
}

#[derive(Clone, Copy, Debug)]
struct BBox {
    min_x: f64,
    min_y: f64,
    max_x: f64,
    max_y: f64,
}

struct EdgePath {
    full_points: Vec<Point>,
    line_points: Vec<Point>,
    arrow_points: Vec<Point>,
    start_offset: f64,
    end_offset: f64,
    source_anchor: Point,
    target_anchor: Point,
    start_dir: Point,
    end_dir: Point,
    #[allow(dead_code)]
    entity_relation_target_ancestor: bool,
}

#[cfg(feature = "edge-debug")]
#[derive(Debug, Serialize)]
pub struct DebugBBox {
    pub min_x: f64,
    pub min_y: f64,
    pub max_x: f64,
    pub max_y: f64,
}

#[cfg(feature = "edge-debug")]
#[derive(Debug, Serialize)]
pub struct DebugVertexBounds {
    pub id: String,
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
    pub rotation: f64,
    pub stroke_width: f64,
    pub is_image: bool,
    pub has_transform: bool,
    pub bbox_before_stroke: DebugBBox,
    pub bbox_after_stroke: DebugBBox,
}

#[cfg(feature = "edge-debug")]
#[derive(Debug, Serialize)]
pub struct DebugEdgeBounds {
    pub id: String,
    pub source_id: Option<String>,
    pub target_id: Option<String>,
    pub shape: Option<String>,
    pub edge_style: Option<String>,
    pub start_arrow: Option<String>,
    pub end_arrow: Option<String>,
    pub stroke_width: f64,
    pub extra_width: f64,
    pub extra_height: f64,
    pub base_bbox: Option<DebugBBox>,
    pub flex_bbox: Option<DebugBBox>,
    pub marker_start_bbox: Option<DebugBBox>,
    pub marker_end_bbox: Option<DebugBBox>,
    pub marker_start_kind: Option<String>,
    pub marker_end_kind: Option<String>,
    pub final_bbox: Option<DebugBBox>,
}

#[cfg(feature = "edge-debug")]
#[derive(Debug, Serialize)]
pub struct DebugGraphBounds {
    pub raw_min_x: f64,
    pub raw_min_y: f64,
    pub raw_max_x: f64,
    pub raw_max_y: f64,
    pub min_x: f64,
    pub min_y: f64,
    pub width: f64,
    pub height: f64,
    pub extra_width: f64,
    pub extra_height: f64,
    pub vertices: Vec<DebugVertexBounds>,
    pub edges: Vec<DebugEdgeBounds>,
}

#[cfg(feature = "edge-debug")]
#[derive(Debug, Serialize)]
pub struct DebugOutlineCorner {
    pub index: usize,
    pub point: (f64, f64),
    pub prev: (f64, f64),
    pub next: (f64, f64),
    pub cross: f64,
    pub concave: bool,
    pub skip: bool,
}

#[cfg(feature = "edge-debug")]
#[derive(Debug, Serialize)]
pub struct DebugEdgeGeometry {
    pub shape: Option<String>,
    pub edge_style: Option<String>,
    pub source_anchor: (f64, f64),
    pub target_anchor: (f64, f64),
    pub start_dir: (f64, f64),
    pub end_dir: (f64, f64),
    pub line_points: Vec<(f64, f64)>,
    pub full_points: Vec<(f64, f64)>,
    pub flex_centerline: Option<Vec<(f64, f64)>>,
    pub flex_outline: Option<Vec<(f64, f64)>>,
    pub flex_outline_skip: Option<Vec<bool>>,
    pub flex_outline_corners: Option<Vec<DebugOutlineCorner>>,
    pub flex_round_indices: Option<Vec<usize>>,
}

struct EdgeRender {
    line: String,
    arrow: String,
    labels: Vec<String>,
}

struct EdgeLineSegment {
    d: String,
    fill: Option<&'static str>,
    linejoin: Option<&'static str>,
    miterlimit: Option<&'static str>,
    pointer_events: &'static str,
}

#[cfg(feature = "edge-debug")]
pub fn debug_edge_geometry(
    edge: &MxCell,
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<Option<DebugEdgeGeometry>> {
    let Some(edge_path) = edge_path_absolute(edge, cell_by_id)? else {
        return Ok(None);
    };
    let style = edge.style.as_deref();
    let shape = style_value(style, "shape").map(|value| value.to_string());
    let edge_style = style_value(style, "edgeStyle").map(|value| value.to_string());
    let line_points = edge_path
        .line_points
        .iter()
        .map(|point| (point.x, point.y))
        .collect();
    let full_points = edge_path
        .full_points
        .iter()
        .map(|point| (point.x, point.y))
        .collect();

    let mut flex_centerline = None;
    let mut flex_outline = None;
    let mut flex_outline_skip = None;
    let mut flex_outline_corners = None;
    let mut flex_round_indices = None;
    if shape.as_deref() == Some("flexArrow") {
        let centerline = flex_arrow_centerline(edge, &edge_path);
        flex_centerline = Some(centerline.iter().map(|point| (point.x, point.y)).collect());
        let start_head = match style_value(style, "startArrow") {
            Some("none") | Some("0") => false,
            Some(_) => true,
            None => false,
        };
        let end_head = !matches!(style_value(style, "endArrow"), Some("none") | Some("0"));
        let metrics = flex_arrow_metrics();
        let use_orthogonal_offsets = style_value(style, "edgeStyle") == Some("orthogonalEdgeStyle");
        let include_end_head_points = true;
        let (outline, skip_round) = flex_arrow_outline(
            &centerline,
            start_head,
            end_head,
            include_end_head_points,
            metrics,
            use_orthogonal_offsets,
        );
        if !outline.is_empty() {
            flex_outline = Some(outline.iter().map(|point| (point.x, point.y)).collect());
            flex_outline_skip = Some(skip_round.clone());
            flex_outline_corners = Some(debug_outline_corners(&outline, &skip_round));
            if use_orthogonal_offsets {
                flex_round_indices = Some(orthogonal_flex_round_indices(
                    &centerline,
                    &outline,
                    metrics.body_half,
                ));
            }
        }
    }

    Ok(Some(DebugEdgeGeometry {
        shape,
        edge_style,
        source_anchor: (edge_path.source_anchor.x, edge_path.source_anchor.y),
        target_anchor: (edge_path.target_anchor.x, edge_path.target_anchor.y),
        start_dir: (edge_path.start_dir.x, edge_path.start_dir.y),
        end_dir: (edge_path.end_dir.x, edge_path.end_dir.y),
        line_points,
        full_points,
        flex_centerline,
        flex_outline,
        flex_outline_skip,
        flex_outline_corners,
        flex_round_indices,
    }))
}

#[cfg(feature = "edge-debug")]
fn debug_outline_corners(points: &[Point], skip_round: &[bool]) -> Vec<DebugOutlineCorner> {
    if points.is_empty() {
        return Vec::new();
    }
    let area = polygon_area(points);
    let orientation = if area.abs() <= f64::EPSILON {
        1.0
    } else if area > 0.0 {
        1.0
    } else {
        -1.0
    };
    let count = points.len();
    let mut corners = Vec::with_capacity(count);
    for idx in 0..count {
        let prev = points[(idx + count - 1) % count];
        let corner = points[idx];
        let next = points[(idx + 1) % count];
        let cross =
            (corner.x - prev.x) * (next.y - corner.y) - (corner.y - prev.y) * (next.x - corner.x);
        let concave = cross * orientation < 0.0;
        let skip = skip_round.get(idx).copied().unwrap_or(false);
        corners.push(DebugOutlineCorner {
            index: idx,
            point: (corner.x, corner.y),
            prev: (prev.x, prev.y),
            next: (next.x, next.y),
            cross,
            concave,
            skip,
        });
    }
    corners
}

fn render_edge(
    edge: &MxCell,
    cell_by_id: &BTreeMap<String, &MxCell>,
    min_x: f64,
    min_y: f64,
    label_cells: &[&MxCell],
) -> SvgResult<Option<EdgeRender>> {
    let Some(edge_path) = edge_path_absolute(edge, cell_by_id)? else {
        return Ok(None);
    };
    let style = edge.style.as_deref();
    let dash_attr = dash_pattern_attr(style);
    let stroke_width_attr = stroke_width_attr(style);
    let (stroke_attr, stroke_style_attr) = edge_stroke_attrs(style);

    let mut line = String::new();
    for segment in edge_line_paths(edge, &edge_path, min_x, min_y) {
        let linejoin = segment
            .linejoin
            .map(|value| format!(" stroke-linejoin=\"{value}\""))
            .unwrap_or_default();
        let miterlimit = segment
            .miterlimit
            .map(|value| format!(" stroke-miterlimit=\"{value}\""))
            .unwrap_or_else(|| " stroke-miterlimit=\"10\"".to_string());
        let fill = segment.fill.unwrap_or("none");
        write!(
            line,
            "<path d=\"{}\" fill=\"{}\" stroke=\"{}\"{} pointer-events=\"{}\"{}{}{}{} />",
            segment.d,
            fill,
            stroke_attr,
            miterlimit,
            segment.pointer_events,
            dash_attr,
            stroke_width_attr,
            stroke_style_attr,
            linejoin
        )
        .unwrap();
    }

    let arrow = render_edge_markers(
        edge,
        &edge_path,
        min_x,
        min_y,
        &stroke_attr,
        &stroke_style_attr,
        &stroke_width_attr,
    );

    let mut labels = Vec::new();
    if let Some(value_label) = render_edge_value_label(edge, &edge_path, min_x, min_y) {
        labels.push(value_label);
    }
    for label in label_cells {
        if let Some(label) =
            render_edge_label_cell(edge, label, cell_by_id, &edge_path, min_x, min_y)
        {
            labels.push(label);
        }
    }

    Ok(Some(EdgeRender {
        line,
        arrow,
        labels,
    }))
}

fn edge_line_paths(
    edge: &MxCell,
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
) -> Vec<EdgeLineSegment> {
    let style = edge.style.as_deref();
    if style_value(style, "shape") == Some("flexArrow") {
        return flex_arrow_paths(edge, edge_path, min_x, min_y);
    }
    if style_value(style, "shape") == Some("arrow")
        && let Some(segment) = arrow_shape_path(edge_path, min_x, min_y)
    {
        return vec![segment];
    }
    if style_value(style, "shape") == Some("link")
        && let Some(segment) = link_shape_path(edge_path, min_x, min_y)
    {
        return vec![segment];
    }
    if style_value(style, "edgeStyle") == Some("entityRelationEdgeStyle")
        && style_value(style, "curved") == Some("1")
    {
        let segment = style_value(style, "segment")
            .and_then(|value| value.parse::<f64>().ok())
            .unwrap_or(20.0);
        return vec![entity_relation_curved_path(
            edge_path, min_x, min_y, segment,
        )];
    }
    if style_value(style, "edgeStyle") == Some("entityRelationEdgeStyle") {
        let segment = style_value(style, "segment")
            .and_then(|value| value.parse::<f64>().ok())
            .unwrap_or(20.0);
        let extra = entity_relation_segment_extra(style);
        return vec![entity_relation_path(
            edge_path, min_x, min_y, segment, extra,
        )];
    }
    if style_value(style, "curved") == Some("1") {
        return vec![curved_edge_path(edge_path, min_x, min_y)];
    }
    if style_value(style, "rounded") != Some("0") && edge_path.line_points.len() > 2 {
        return vec![rounded_edge_path(edge_path, min_x, min_y)];
    }

    let mut d = String::new();
    for (idx, point) in edge_path.line_points.iter().enumerate() {
        let x = point.x - min_x;
        let y = point.y - min_y;
        if idx == 0 {
            write!(d, "M {} {}", fmt_num(x), fmt_num(y)).unwrap();
        } else {
            write!(d, " L {} {}", fmt_num(x), fmt_num(y)).unwrap();
        }
    }
    vec![EdgeLineSegment {
        d,
        fill: None,
        linejoin: None,
        miterlimit: None,
        pointer_events: "stroke",
    }]
}

fn entity_relation_segment_extra(style: Option<&str>) -> f64 {
    let stroke_width = stroke_width_value(style);
    let start_kind = marker_kind_from_value(style_value(style, "startArrow"));
    let end_kind = match style_value(style, "endArrow") {
        Some("none") => None,
        Some(value) => marker_kind_from_value(Some(value)),
        None => Some(MarkerKind::Classic),
    };
    let mut extra = 0.0;
    for kind in [start_kind, end_kind] {
        let value = match kind {
            Some(MarkerKind::Circle) | Some(MarkerKind::CirclePlus) => 2.0 * stroke_width,
            Some(MarkerKind::ERzeroToOne) | Some(MarkerKind::ERzeroToMany) => {
                1.25 * stroke_width + 0.5
            }
            Some(MarkerKind::DoubleBlock) => {
                let (tip_gap, arrow_length, arrow_back, _) = edge_arrow_metrics(stroke_width);
                arrow_length + arrow_back - tip_gap + 0.03
            }
            _ => 0.0,
        };
        if value > extra {
            extra = value;
        }
    }
    extra
}

fn flex_arrow_paths(
    edge: &MxCell,
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
) -> Vec<EdgeLineSegment> {
    let style = edge.style.as_deref();
    let points = flex_arrow_centerline(edge, edge_path);
    if points.len() < 2 {
        return Vec::new();
    }
    let start_head = match style_value(style, "startArrow") {
        Some("none") | Some("0") => false,
        Some(_) => true,
        None => false,
    };
    let end_head = !matches!(style_value(style, "endArrow"), Some("none") | Some("0"));
    let metrics = flex_arrow_metrics();
    let use_orthogonal_offsets = style_value(style, "edgeStyle") == Some("orthogonalEdgeStyle");
    let rounded = style_value(style, "edgeStyle") == Some("orthogonalEdgeStyle")
        || style_value(style, "edgeStyle") == Some("entityRelationEdgeStyle");
    let include_end_head_points = true;
    let (outline, _round_skip) = flex_arrow_outline(
        &points,
        start_head,
        end_head,
        include_end_head_points,
        metrics,
        use_orthogonal_offsets,
    );
    if outline.is_empty() {
        return Vec::new();
    }
    let main = if rounded && outline.len() > 2 {
        if style_value(style, "edgeStyle") == Some("orthogonalEdgeStyle") {
            let adjusted = adjust_outline_min_x(&outline, 0.5);
            let round_indices =
                orthogonal_flex_round_indices(&points, &adjusted, metrics.body_half);
            rounded_polygon_path_indices(
                &adjusted,
                &round_indices,
                metrics.corner_radius,
                min_x,
                min_y,
            )
        } else {
            rounded_polygon_path(&outline, metrics.corner_radius, min_x, min_y)
        }
    } else {
        polygon_path(&outline, min_x, min_y)
    };
    let mut out = vec![EdgeLineSegment {
        d: main,
        fill: None,
        linejoin: Some("round"),
        miterlimit: Some("10"),
        pointer_events: "all",
    }];
    if rounded
        && end_head
        && let Some(head) = flex_arrow_head_path(
            points.last().copied().unwrap(),
            points[points.len() - 2],
            metrics,
            min_x,
            min_y,
        )
    {
        out.push(EdgeLineSegment {
            d: head,
            fill: None,
            linejoin: Some("flat"),
            miterlimit: Some("4"),
            pointer_events: "all",
        });
    }
    out
}

fn edge_path_absolute(
    edge: &MxCell,
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<Option<EdgePath>> {
    let geometry = edge.geometry.as_ref();
    let mut control_points = Vec::new();
    let mut source_point = None;
    let mut target_point = None;

    if let Some(geom) = geometry {
        if edge.source.is_none()
            && let Some(point) = geom.source_point.as_ref().and_then(point_from_mxpoint)
        {
            source_point = Some(point);
        }
        if edge.target.is_none()
            && let Some(point) = geom.target_point.as_ref().and_then(point_from_mxpoint)
        {
            target_point = Some(point);
        }
        for point in &geom.points {
            if let Some(point) = point_from_mxpoint(point) {
                control_points.push(point);
            }
        }
    }

    let source_raw = edge
        .source
        .as_ref()
        .and_then(|source_id| cell_by_id.get(source_id).copied());
    let target_raw = edge
        .target
        .as_ref()
        .and_then(|target_id| cell_by_id.get(target_id).copied());
    let source = source_raw.and_then(|cell| resolve_visible_terminal(cell, cell_by_id));
    let target = target_raw.and_then(|cell| resolve_visible_terminal(cell, cell_by_id));
    let target_hint_center = match (target_raw, target) {
        (Some(raw), Some(resolved)) if raw.id != resolved.id => cell_center(raw, cell_by_id).ok(),
        _ => None,
    };

    if source_point.is_none() && target_point.is_none() && source.is_none() && target.is_none() {
        return Ok(None);
    }

    if source_point.is_none()
        && let Some(source_cell) = source
    {
        if let Some(point) =
            terminal_point_override(edge.style.as_deref(), source_cell, true, cell_by_id)?
        {
            source_point = Some(point);
        } else {
            let edge_style = style_value(edge.style.as_deref(), "edgeStyle");
            let use_orthogonal_terminal = (matches!(
                edge_style,
                Some("orthogonalEdgeStyle") | Some("elbowEdgeStyle")
            ) && !is_ellipse(source_cell.style.as_deref()))
                || (edge_style == Some("entityRelationEdgeStyle")
                    && !is_partial_rectangle(source_cell.style.as_deref()));
            let source_center = cell_center(source_cell, cell_by_id)?;
            let mut target_dir = if use_orthogonal_terminal {
                if let Some(hint_center) = target_hint_center {
                    Point {
                        x: hint_center.x - source_center.x,
                        y: hint_center.y - source_center.y,
                    }
                } else if let Some(target_cell) = target {
                    let target_center = cell_center(target_cell, cell_by_id)?;
                    Point {
                        x: target_center.x - source_center.x,
                        y: target_center.y - source_center.y,
                    }
                } else if let Some(first) = control_points.first() {
                    Point {
                        x: first.x - source_center.x,
                        y: first.y - source_center.y,
                    }
                } else {
                    Point { x: 1.0, y: 0.0 }
                }
            } else if let Some(first) = control_points.first() {
                Point {
                    x: first.x - source_center.x,
                    y: first.y - source_center.y,
                }
            } else if let Some(hint_center) = target_hint_center {
                Point {
                    x: hint_center.x - source_center.x,
                    y: hint_center.y - source_center.y,
                }
            } else if let Some(target_cell) = target {
                let target_center = cell_center(target_cell, cell_by_id)?;
                Point {
                    x: target_center.x - source_center.x,
                    y: target_center.y - source_center.y,
                }
            } else {
                Point { x: 1.0, y: 0.0 }
            };
            if use_orthogonal_terminal {
                target_dir = constrain_port_direction(edge.style.as_deref(), true, target_dir);
            }
            let spacing = perimeter_spacing(source_cell.style.as_deref());
            source_point = Some(if use_orthogonal_terminal {
                orthogonal_terminal_point_for_cell(source_cell, target_dir, spacing, cell_by_id)?
            } else {
                terminal_point_for_cell(source_cell, target_dir, spacing, cell_by_id)?
            });
        }
    }

    if target_point.is_none()
        && let Some(target_cell) = target
    {
        if let Some(point) =
            terminal_point_override(edge.style.as_deref(), target_cell, false, cell_by_id)?
        {
            target_point = Some(point);
        } else {
            let edge_style = style_value(edge.style.as_deref(), "edgeStyle");
            let use_orthogonal_terminal = (matches!(
                edge_style,
                Some("orthogonalEdgeStyle") | Some("elbowEdgeStyle")
            ) && !is_ellipse(target_cell.style.as_deref()))
                || (edge_style == Some("entityRelationEdgeStyle")
                    && !is_partial_rectangle(target_cell.style.as_deref()));
            let target_center = cell_center(target_cell, cell_by_id)?;
            let target_hint_dir = target_hint_center.map(|hint_center| Point {
                x: hint_center.x - target_center.x,
                y: hint_center.y - target_center.y,
            });
            let source_has_exit = style_value(edge.style.as_deref(), "exitX").is_some()
                && style_value(edge.style.as_deref(), "exitY").is_some();
            let mut source_dir = if use_orthogonal_terminal {
                if source_has_exit {
                    if let Some(source_anchor) = source_point {
                        Point {
                            x: source_anchor.x - target_center.x,
                            y: source_anchor.y - target_center.y,
                        }
                    } else if let Some(source_cell) = source {
                        let source_center = cell_center(source_cell, cell_by_id)?;
                        Point {
                            x: source_center.x - target_center.x,
                            y: source_center.y - target_center.y,
                        }
                    } else if let Some(last) = control_points.last() {
                        Point {
                            x: last.x - target_center.x,
                            y: last.y - target_center.y,
                        }
                    } else {
                        Point { x: -1.0, y: 0.0 }
                    }
                } else if let Some(hint_dir) = target_hint_dir {
                    hint_dir
                } else if let Some(source_cell) = source {
                    let source_center = cell_center(source_cell, cell_by_id)?;
                    Point {
                        x: source_center.x - target_center.x,
                        y: source_center.y - target_center.y,
                    }
                } else if let Some(source_anchor) = source_point {
                    Point {
                        x: source_anchor.x - target_center.x,
                        y: source_anchor.y - target_center.y,
                    }
                } else if let Some(last) = control_points.last() {
                    Point {
                        x: last.x - target_center.x,
                        y: last.y - target_center.y,
                    }
                } else {
                    Point { x: -1.0, y: 0.0 }
                }
            } else if let Some(hint_dir) = target_hint_dir {
                hint_dir
            } else if let Some(last) = control_points.last() {
                Point {
                    x: last.x - target_center.x,
                    y: last.y - target_center.y,
                }
            } else if let Some(source_anchor) = source_point {
                Point {
                    x: source_anchor.x - target_center.x,
                    y: source_anchor.y - target_center.y,
                }
            } else if let Some(source_cell) = source {
                let source_center = cell_center(source_cell, cell_by_id)?;
                Point {
                    x: source_center.x - target_center.x,
                    y: source_center.y - target_center.y,
                }
            } else {
                Point { x: -1.0, y: 0.0 }
            };
            if use_orthogonal_terminal {
                source_dir = constrain_port_direction(edge.style.as_deref(), false, source_dir);
            }
            target_point = Some(if use_orthogonal_terminal {
                orthogonal_terminal_point_for_cell(target_cell, source_dir, 0.0, cell_by_id)?
            } else {
                terminal_point_for_cell(target_cell, source_dir, 0.0, cell_by_id)?
            });
        }
    }

    let Some(start_anchor) = source_point else {
        return Ok(None);
    };
    let Some(end_anchor) = target_point else {
        return Ok(None);
    };

    let style = edge.style.as_deref();
    let (start_offset, end_offset) = marker_offsets(style, stroke_width_value(style));

    let mut start_dir = if let Some(first) = control_points.first() {
        normalize(Point {
            x: first.x - start_anchor.x,
            y: first.y - start_anchor.y,
        })
    } else {
        normalize(Point {
            x: end_anchor.x - start_anchor.x,
            y: end_anchor.y - start_anchor.y,
        })
    };
    let mut end_dir = if let Some(last) = control_points.last() {
        normalize(Point {
            x: end_anchor.x - last.x,
            y: end_anchor.y - last.y,
        })
    } else {
        normalize(Point {
            x: end_anchor.x - start_anchor.x,
            y: end_anchor.y - start_anchor.y,
        })
    };
    if style_value(style, "edgeStyle") == Some("entityRelationEdgeStyle") {
        let axis_dir = axis_dir_from_points(start_anchor, end_anchor);
        if source.is_none() && target.is_none() {
            start_dir = axis_dir;
            end_dir = axis_dir;
        }
        if let Some(source_cell) = source {
            start_dir = axis_dir_from_anchor(source_cell, start_anchor, cell_by_id)?;
        }
        if let Some(target_cell) = target {
            let target_dir = axis_dir_from_anchor(target_cell, end_anchor, cell_by_id)?;
            end_dir = Point {
                x: -target_dir.x,
                y: -target_dir.y,
            };
        }
    }

    let start = Point {
        x: start_anchor.x + start_dir.x * start_offset,
        y: start_anchor.y + start_dir.y * start_offset,
    };
    let end = Point {
        x: end_anchor.x - end_dir.x * end_offset,
        y: end_anchor.y - end_dir.y * end_offset,
    };

    let mut full_points = Vec::with_capacity(control_points.len() + 2);
    full_points.push(start);
    full_points.extend(control_points.iter().copied());
    full_points.push(end);

    let mut line_points = full_points.clone();
    if style_value(style, "edgeStyle") == Some("elbowEdgeStyle")
        && let Some(control) = control_points.first()
    {
        let elbow = style_value(style, "elbow");
        line_points = if elbow == Some("horizontal") {
            vec![
                start,
                Point {
                    x: control.x,
                    y: start.y,
                },
                Point {
                    x: control.x,
                    y: end.y,
                },
                end,
            ]
        } else {
            vec![
                start,
                Point {
                    x: start.x,
                    y: control.y,
                },
                Point {
                    x: end.x,
                    y: control.y,
                },
                end,
            ]
        };
        full_points = line_points.clone();
    } else if style_value(style, "edgeStyle") == Some("orthogonalEdgeStyle")
        && control_points.is_empty()
    {
        line_points = vec![
            start,
            Point {
                x: start.x,
                y: end.y,
            },
            end,
        ];
        full_points = line_points.clone();
    }
    let arrow_points = Vec::new();

    let entity_relation_target_ancestor =
        if style_value(style, "edgeStyle") == Some("entityRelationEdgeStyle") {
            match (source, target) {
                (Some(source_cell), Some(target_cell)) => {
                    is_ancestor(target_cell, source_cell, cell_by_id)
                }
                _ => false,
            }
        } else {
            false
        };

    Ok(Some(EdgePath {
        full_points,
        line_points,
        arrow_points,
        start_offset,
        end_offset,
        source_anchor: start_anchor,
        target_anchor: end_anchor,
        start_dir,
        end_dir,
        entity_relation_target_ancestor,
    }))
}

fn cell_center(cell: &MxCell, cell_by_id: &BTreeMap<String, &MxCell>) -> SvgResult<Point> {
    let geometry = cell
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
    let offset = parent_offset(cell, cell_by_id);
    let x = geometry.x.unwrap_or(0.0) + offset.x;
    let y = geometry.y.unwrap_or(0.0) + offset.y;
    let width = geometry.width.unwrap_or(0.0);
    let height = geometry.height.unwrap_or(0.0);
    Ok(Point {
        x: x + width / 2.0,
        y: y + height / 2.0,
    })
}

fn terminal_point_for_cell(
    cell: &MxCell,
    direction: Point,
    spacing: f64,
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<Point> {
    let geometry = cell
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
    let offset = parent_offset(cell, cell_by_id);
    let x = geometry.x.unwrap_or(0.0) + offset.x;
    let y = geometry.y.unwrap_or(0.0) + offset.y;
    let width = geometry.width.unwrap_or(0.0);
    let height = geometry.height.unwrap_or(0.0);
    let rotation = rotation_degrees(cell.style.as_deref());
    let half_w = width / 2.0 + spacing;
    let half_h = height / 2.0 + spacing;
    let center = Point {
        x: x + width / 2.0,
        y: y + height / 2.0,
    };
    if is_partial_rectangle(cell.style.as_deref())
        && style_value(cell.style.as_deref(), "bottom") == Some("1")
    {
        let x = if direction.x >= 0.0 { x + width } else { x };
        let y = y + height + spacing;
        return Ok(Point { x, y });
    }
    if is_ellipse(cell.style.as_deref()) {
        Ok(ellipse_intersection(
            center, half_w, half_h, rotation, direction,
        ))
    } else {
        Ok(rect_intersection(
            center, half_w, half_h, rotation, direction,
        ))
    }
}

fn orthogonal_terminal_point_for_cell(
    cell: &MxCell,
    direction: Point,
    spacing: f64,
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<Point> {
    let geometry = cell
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
    let offset = parent_offset(cell, cell_by_id);
    let x = geometry.x.unwrap_or(0.0) + offset.x;
    let y = geometry.y.unwrap_or(0.0) + offset.y;
    let width = geometry.width.unwrap_or(0.0);
    let height = geometry.height.unwrap_or(0.0);
    let center = Point {
        x: x + width / 2.0,
        y: y + height / 2.0,
    };
    let (mut point, rotation) = if direction.x.abs() >= direction.y.abs() {
        if direction.x >= 0.0 {
            (
                Point {
                    x: x + width + spacing,
                    y: y + height / 2.0,
                },
                rotation_degrees(cell.style.as_deref()),
            )
        } else {
            (
                Point {
                    x: x - spacing,
                    y: y + height / 2.0,
                },
                rotation_degrees(cell.style.as_deref()),
            )
        }
    } else if direction.y >= 0.0 {
        (
            Point {
                x: x + width / 2.0,
                y: y + height + spacing,
            },
            rotation_degrees(cell.style.as_deref()),
        )
    } else {
        (
            Point {
                x: x + width / 2.0,
                y: y - spacing,
            },
            rotation_degrees(cell.style.as_deref()),
        )
    };
    if rotation.abs() > f64::EPSILON {
        let angle = rotation.to_radians();
        let dx = point.x - center.x;
        let dy = point.y - center.y;
        point = Point {
            x: center.x + dx * angle.cos() - dy * angle.sin(),
            y: center.y + dx * angle.sin() + dy * angle.cos(),
        };
    }
    Ok(point)
}

fn constrain_port_direction(edge_style: Option<&str>, is_source: bool, direction: Point) -> Point {
    let constraint = if is_source {
        style_value(edge_style, "sourcePortConstraint")
    } else {
        style_value(edge_style, "targetPortConstraint")
    };
    match constraint {
        Some("east") => Point { x: 1.0, y: 0.0 },
        Some("west") => Point { x: -1.0, y: 0.0 },
        Some("north") => Point { x: 0.0, y: -1.0 },
        Some("south") => Point { x: 0.0, y: 1.0 },
        Some("eastwest") => {
            let sign = if direction.x >= 0.0 { 1.0 } else { -1.0 };
            Point { x: sign, y: 0.0 }
        }
        Some("northsouth") => {
            let sign = if direction.y >= 0.0 { 1.0 } else { -1.0 };
            Point { x: 0.0, y: sign }
        }
        _ => direction,
    }
}

fn terminal_point_override(
    edge_style: Option<&str>,
    cell: &MxCell,
    is_source: bool,
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<Option<Point>> {
    let geometry = cell
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
    let rel_x = if is_source {
        style_value(edge_style, "exitX")
    } else {
        style_value(edge_style, "entryX")
    }
    .and_then(|value| value.parse::<f64>().ok());
    let rel_y = if is_source {
        style_value(edge_style, "exitY")
    } else {
        style_value(edge_style, "entryY")
    }
    .and_then(|value| value.parse::<f64>().ok());
    let (Some(rel_x), Some(rel_y)) = (rel_x, rel_y) else {
        return Ok(None);
    };
    let offset = parent_offset(cell, cell_by_id);
    let x = geometry.x.unwrap_or(0.0) + offset.x;
    let y = geometry.y.unwrap_or(0.0) + offset.y;
    let width = geometry.width.unwrap_or(0.0);
    let height = geometry.height.unwrap_or(0.0);
    let rotation = rotation_degrees(cell.style.as_deref());
    let center = Point {
        x: x + width / 2.0,
        y: y + height / 2.0,
    };
    let mut point = Point {
        x: x + rel_x * width,
        y: y + rel_y * height,
    };
    if rotation.abs() > f64::EPSILON {
        let angle = rotation.to_radians();
        let dx = point.x - center.x;
        let dy = point.y - center.y;
        point = Point {
            x: center.x + dx * angle.cos() - dy * angle.sin(),
            y: center.y + dx * angle.sin() + dy * angle.cos(),
        };
    }
    Ok(Some(point))
}

fn axis_dir_from_anchor(
    cell: &MxCell,
    anchor: Point,
    cell_by_id: &BTreeMap<String, &MxCell>,
) -> SvgResult<Point> {
    let center = cell_center(cell, cell_by_id)?;
    let dx = anchor.x - center.x;
    let dy = anchor.y - center.y;
    if dx.abs() >= dy.abs() {
        let sign = if dx >= 0.0 { 1.0 } else { -1.0 };
        Ok(Point { x: sign, y: 0.0 })
    } else {
        let sign = if dy >= 0.0 { 1.0 } else { -1.0 };
        Ok(Point { x: 0.0, y: sign })
    }
}

fn point_from_mxpoint(point: &MxPoint) -> Option<Point> {
    Some(Point {
        x: point.x?,
        y: point.y?,
    })
}

fn point_along_polyline(points: &[Point], fraction: f64) -> Option<(Point, Point)> {
    if points.len() < 2 {
        return None;
    }
    let mut total = 0.0;
    for window in points.windows(2) {
        total += distance(window[0], window[1]);
    }
    if total == 0.0 {
        return Some((points[0], Point { x: 1.0, y: 0.0 }));
    }
    let mut remaining = total * fraction.clamp(0.0, 1.0);
    for window in points.windows(2) {
        let seg_len = distance(window[0], window[1]);
        if seg_len == 0.0 {
            continue;
        }
        if remaining <= seg_len {
            let t = remaining / seg_len;
            let point = Point {
                x: window[0].x + (window[1].x - window[0].x) * t,
                y: window[0].y + (window[1].y - window[0].y) * t,
            };
            let dir = normalize(Point {
                x: window[1].x - window[0].x,
                y: window[1].y - window[0].y,
            });
            return Some((point, dir));
        }
        remaining -= seg_len;
    }
    let last = *points.last()?;
    let prev = points[points.len() - 2];
    let dir = normalize(Point {
        x: last.x - prev.x,
        y: last.y - prev.y,
    });
    Some((last, dir))
}

fn polyline_length(points: &[Point]) -> f64 {
    if points.len() < 2 {
        return 0.0;
    }
    let mut total = 0.0;
    for window in points.windows(2) {
        total += distance(window[0], window[1]);
    }
    total
}

fn distance(a: Point, b: Point) -> f64 {
    ((a.x - b.x).powi(2) + (a.y - b.y).powi(2)).sqrt()
}

fn normalize(point: Point) -> Point {
    let len = (point.x * point.x + point.y * point.y).sqrt();
    if len == 0.0 {
        Point { x: 1.0, y: 0.0 }
    } else {
        Point {
            x: point.x / len,
            y: point.y / len,
        }
    }
}

fn dash_pattern_attr(style: Option<&str>) -> String {
    if let Some(pattern) = style_value(style, "dashPattern") {
        let scale = stroke_width_value(style);
        if (scale - 1.0).abs() <= f64::EPSILON {
            return format!(" stroke-dasharray=\"{pattern}\"");
        }
        let mut parts = Vec::new();
        for part in pattern.split_whitespace() {
            if let Ok(value) = part.parse::<f64>() {
                parts.push(fmt_num(value * scale));
            } else {
                parts.push(part.to_string());
            }
        }
        return format!(" stroke-dasharray=\"{}\"", parts.join(" "));
    }
    if is_dashed(style) {
        let scale = stroke_width_value(style);
        if (scale - 1.0).abs() <= f64::EPSILON {
            return " stroke-dasharray=\"3 3\"".to_string();
        }
        return format!(
            " stroke-dasharray=\"{} {}\"",
            fmt_num(3.0 * scale),
            fmt_num(3.0 * scale)
        );
    }
    String::new()
}

fn edge_stroke_attrs(style: Option<&str>) -> (String, String) {
    let stroke_color = style_value(style, "strokeColor");
    let stroke = match stroke_color {
        Some("none") => "none".to_string(),
        Some(value) => {
            if let Some(pair) = light_dark_pair(value) {
                normalize_color_value(&pair.light)
            } else {
                normalize_color_value(value)
            }
        }
        None => "#000000".to_string(),
    };
    let mut style_parts: Vec<String> = Vec::new();
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
    (stroke, style_attr)
}

fn curved_edge_path(edge_path: &EdgePath, min_x: f64, min_y: f64) -> EdgeLineSegment {
    let mut points = edge_path.full_points.clone();
    if let (Some(last), Some(end)) = (points.last_mut(), edge_path.line_points.last()) {
        *last = *end;
    }
    let mut d = String::new();
    if let Some(first) = points.first() {
        write!(
            d,
            "M {} {}",
            fmt_num(first.x - min_x),
            fmt_num(first.y - min_y)
        )
        .unwrap();
    }
    if points.len() >= 3 {
        for window in points[1..points.len() - 1].windows(2) {
            let control = window[0];
            let end = Point {
                x: (window[0].x + window[1].x) / 2.0,
                y: (window[0].y + window[1].y) / 2.0,
            };
            write!(
                d,
                " Q {} {} {} {}",
                fmt_num(control.x - min_x),
                fmt_num(control.y - min_y),
                fmt_num(end.x - min_x),
                fmt_num(end.y - min_y)
            )
            .unwrap();
        }
        let control = points[points.len() - 2];
        let last = points[points.len() - 1];
        write!(
            d,
            " Q {} {} {} {}",
            fmt_num(control.x - min_x),
            fmt_num(control.y - min_y),
            fmt_num(last.x - min_x),
            fmt_num(last.y - min_y)
        )
        .unwrap();
    }
    EdgeLineSegment {
        d,
        fill: None,
        linejoin: None,
        miterlimit: None,
        pointer_events: "stroke",
    }
}

fn rounded_edge_path(edge_path: &EdgePath, min_x: f64, min_y: f64) -> EdgeLineSegment {
    let radius: f64 = 10.0;
    let points = &edge_path.line_points;
    let mut d = String::new();
    if points.len() < 2 {
        return EdgeLineSegment {
            d,
            fill: None,
            linejoin: None,
            miterlimit: None,
            pointer_events: "stroke",
        };
    }
    let first = points[0];
    write!(
        d,
        "M {} {}",
        fmt_num(first.x - min_x),
        fmt_num(first.y - min_y)
    )
    .unwrap();
    if points.len() == 2 {
        let last = points[1];
        write!(
            d,
            " L {} {}",
            fmt_num(last.x - min_x),
            fmt_num(last.y - min_y)
        )
        .unwrap();
        return EdgeLineSegment {
            d,
            fill: None,
            linejoin: None,
            miterlimit: None,
            pointer_events: "stroke",
        };
    }
    let mut current = first;
    for window in points.windows(3) {
        let prev = window[0];
        let corner = window[1];
        let next = window[2];
        let in_vec = Point {
            x: prev.x - corner.x,
            y: prev.y - corner.y,
        };
        let out_vec = Point {
            x: next.x - corner.x,
            y: next.y - corner.y,
        };
        let in_len = (in_vec.x * in_vec.x + in_vec.y * in_vec.y).sqrt();
        let out_len = (out_vec.x * out_vec.x + out_vec.y * out_vec.y).sqrt();
        if in_len <= f64::EPSILON || out_len <= f64::EPSILON {
            continue;
        }
        let r = radius.min(in_len / 2.0).min(out_len / 2.0);
        if r <= f64::EPSILON {
            if (current.x - corner.x).abs() > f64::EPSILON
                || (current.y - corner.y).abs() > f64::EPSILON
            {
                write!(
                    d,
                    " L {} {}",
                    fmt_num(corner.x - min_x),
                    fmt_num(corner.y - min_y)
                )
                .unwrap();
                current = corner;
            }
            continue;
        }
        let in_unit = Point {
            x: in_vec.x / in_len,
            y: in_vec.y / in_len,
        };
        let out_unit = Point {
            x: out_vec.x / out_len,
            y: out_vec.y / out_len,
        };
        let pre = Point {
            x: corner.x + in_unit.x * r,
            y: corner.y + in_unit.y * r,
        };
        let post = Point {
            x: corner.x + out_unit.x * r,
            y: corner.y + out_unit.y * r,
        };
        if (current.x - pre.x).abs() > f64::EPSILON || (current.y - pre.y).abs() > f64::EPSILON {
            write!(
                d,
                " L {} {}",
                fmt_num(pre.x - min_x),
                fmt_num(pre.y - min_y)
            )
            .unwrap();
        }
        write!(
            d,
            " Q {} {} {} {}",
            fmt_num(corner.x - min_x),
            fmt_num(corner.y - min_y),
            fmt_num(post.x - min_x),
            fmt_num(post.y - min_y)
        )
        .unwrap();
        current = post;
    }
    let last = points[points.len() - 1];
    write!(
        d,
        " L {} {}",
        fmt_num(last.x - min_x),
        fmt_num(last.y - min_y)
    )
    .unwrap();
    EdgeLineSegment {
        d,
        fill: None,
        linejoin: None,
        miterlimit: None,
        pointer_events: "stroke",
    }
}

fn entity_relation_path(
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
    segment: f64,
    extra: f64,
) -> EdgeLineSegment {
    let points = entity_relation_points(edge_path, segment);

    if points.len() < 2 {
        // Fallback to straight line if points insufficient.
        let start = edge_path
            .line_points
            .first()
            .copied()
            .unwrap_or(edge_path.source_anchor);
        let end = edge_path
            .line_points
            .last()
            .copied()
            .unwrap_or(edge_path.target_anchor);
        let mut d = String::new();
        write!(
            d,
            "M {} {} L {} {}",
            fmt_num(start.x - min_x),
            fmt_num(start.y - min_y),
            fmt_num(end.x - min_x),
            fmt_num(end.y - min_y)
        )
        .unwrap();
        return EdgeLineSegment {
            d,
            fill: None,
            linejoin: None,
            miterlimit: None,
            pointer_events: "stroke",
        };
    }

    let mut d = String::new();
    let write_point = |point: Point, out: &mut String| {
        write!(
            out,
            " {} {}",
            fmt_num(point.x - min_x),
            fmt_num(point.y - min_y)
        )
        .unwrap();
    };
    let first = points[0];
    write!(
        d,
        "M {} {}",
        fmt_num(first.x - min_x),
        fmt_num(first.y - min_y)
    )
    .unwrap();

    let dir = entity_relation_dir(edge_path);
    let source_segment = if points.len() > 1 && extra != 0.0 {
        Point {
            x: points[1].x + dir.x * extra,
            y: points[1].y + dir.y * extra,
        }
    } else {
        points.get(1).copied().unwrap_or(points[0])
    };
    let target_segment = if points.len() > 4 && extra != 0.0 {
        Point {
            x: points[4].x - dir.x * extra,
            y: points[4].y - dir.y * extra,
        }
    } else {
        points.get(4).copied().unwrap_or(*points.last().unwrap())
    };

    if points.len() > 1 {
        write!(d, " L").unwrap();
        write_point(source_segment, &mut d);
    }
    if points.len() > 2 {
        let half = segment / 2.0;
        let control = if dir.x.abs() >= dir.y.abs() {
            Point {
                x: points[1].x + dir.x * half,
                y: points[1].y,
            }
        } else {
            Point {
                x: points[1].x,
                y: points[1].y + dir.y * half,
            }
        };
        write!(
            d,
            " Q {} {} {} {}",
            fmt_num(control.x - min_x),
            fmt_num(control.y - min_y),
            fmt_num(points[2].x - min_x),
            fmt_num(points[2].y - min_y)
        )
        .unwrap();
    }
    if points.len() > 3 {
        write!(d, " L").unwrap();
        write_point(points[3], &mut d);
    }
    if points.len() > 4 {
        let half = segment / 2.0;
        let control = if dir.x.abs() >= dir.y.abs() {
            Point {
                x: points[4].x - dir.x * half,
                y: points[4].y,
            }
        } else {
            Point {
                x: points[4].x,
                y: points[4].y - dir.y * half,
            }
        };
        write!(
            d,
            " Q {} {} {} {}",
            fmt_num(control.x - min_x),
            fmt_num(control.y - min_y),
            fmt_num(target_segment.x - min_x),
            fmt_num(target_segment.y - min_y)
        )
        .unwrap();
    }
    if points.len() > 5 {
        write!(d, " L").unwrap();
        write_point(points[5], &mut d);
    }
    EdgeLineSegment {
        d,
        fill: None,
        linejoin: None,
        miterlimit: None,
        pointer_events: "stroke",
    }
}

fn entity_relation_curved_path(
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
    segment: f64,
) -> EdgeLineSegment {
    let start = edge_path
        .line_points
        .first()
        .copied()
        .unwrap_or(edge_path.source_anchor);
    let end = edge_path
        .line_points
        .last()
        .copied()
        .unwrap_or(edge_path.target_anchor);
    let mut control_start = Point {
        x: start.x + edge_path.start_dir.x * segment,
        y: start.y + edge_path.start_dir.y * segment,
    };
    let control_end = Point {
        x: end.x - edge_path.end_dir.x * segment,
        y: end.y - edge_path.end_dir.y * segment,
    };

    let same_dir = (edge_path.start_dir.x * edge_path.end_dir.x
        + edge_path.start_dir.y * edge_path.end_dir.y)
        > 0.0;
    let horizontal = edge_path.start_dir.x.abs() >= edge_path.start_dir.y.abs();
    if !same_dir {
        control_start = if horizontal {
            Point {
                x: control_end.x,
                y: start.y,
            }
        } else {
            Point {
                x: start.x,
                y: control_end.y,
            }
        };
    }
    let mid = if same_dir {
        Point {
            x: (control_start.x + control_end.x) / 2.0,
            y: (control_start.y + control_end.y) / 2.0,
        }
    } else if horizontal {
        Point {
            x: control_end.x,
            y: (start.y + end.y) / 2.0,
        }
    } else {
        Point {
            x: (start.x + end.x) / 2.0,
            y: control_end.y,
        }
    };

    let mut d = String::new();
    write!(
        d,
        "M {} {}",
        fmt_num(start.x - min_x),
        fmt_num(start.y - min_y)
    )
    .unwrap();
    write!(
        d,
        " Q {} {} {} {}",
        fmt_num(control_start.x - min_x),
        fmt_num(control_start.y - min_y),
        fmt_num(mid.x - min_x),
        fmt_num(mid.y - min_y)
    )
    .unwrap();
    write!(
        d,
        " Q {} {} {} {}",
        fmt_num(control_end.x - min_x),
        fmt_num(control_end.y - min_y),
        fmt_num(end.x - min_x),
        fmt_num(end.y - min_y)
    )
    .unwrap();
    EdgeLineSegment {
        d,
        fill: None,
        linejoin: None,
        miterlimit: None,
        pointer_events: "stroke",
    }
}

fn arrow_shape_path(edge_path: &EdgePath, min_x: f64, min_y: f64) -> Option<EdgeLineSegment> {
    let start = edge_path.source_anchor;
    let end = edge_path.target_anchor;
    let dir = normalize(Point {
        x: end.x - start.x,
        y: end.y - start.y,
    });
    if dir.x == 0.0 && dir.y == 0.0 {
        return None;
    }
    let perp = Point {
        x: -dir.y,
        y: dir.x,
    };
    let body_half = 5.0;
    let head_half = 15.0;
    let head_length = 30.0;
    let body_end = Point {
        x: end.x - dir.x * head_length,
        y: end.y - dir.y * head_length,
    };
    let points = [
        Point {
            x: start.x + perp.x * body_half,
            y: start.y + perp.y * body_half,
        },
        Point {
            x: start.x - perp.x * body_half,
            y: start.y - perp.y * body_half,
        },
        Point {
            x: body_end.x - perp.x * body_half,
            y: body_end.y - perp.y * body_half,
        },
        Point {
            x: body_end.x - perp.x * head_half,
            y: body_end.y - perp.y * head_half,
        },
        end,
        Point {
            x: body_end.x + perp.x * head_half,
            y: body_end.y + perp.y * head_half,
        },
        Point {
            x: body_end.x + perp.x * body_half,
            y: body_end.y + perp.y * body_half,
        },
    ];
    let mut d = String::new();
    write!(
        d,
        "M {} {}",
        fmt_num(points[0].x - min_x),
        fmt_num(points[0].y - min_y)
    )
    .unwrap();
    for point in &points[1..] {
        write!(
            d,
            " L {} {}",
            fmt_num(point.x - min_x),
            fmt_num(point.y - min_y)
        )
        .unwrap();
    }
    d.push_str(" Z");
    Some(EdgeLineSegment {
        d,
        fill: None,
        linejoin: None,
        miterlimit: None,
        pointer_events: "all",
    })
}

#[derive(Clone, Copy)]
struct FlexArrowMetrics {
    body_half: f64,
    head_half: f64,
    head_length: f64,
    tip_gap: f64,
    corner_radius: f64,
}

fn flex_arrow_metrics() -> FlexArrowMetrics {
    FlexArrowMetrics {
        body_half: 5.0,
        head_half: 15.5,
        head_length: 19.5,
        tip_gap: 0.5,
        corner_radius: 10.0,
    }
}

fn flex_arrow_centerline(edge: &MxCell, edge_path: &EdgePath) -> Vec<Point> {
    let source = edge_path.source_anchor;
    let target = edge_path.target_anchor;
    let style = edge.style.as_deref();
    if edge_path.line_points.len() > 2
        && (style_value(style, "edgeStyle") != Some("orthogonalEdgeStyle")
            || edge_path.line_points.len() != 3)
    {
        return edge_path.line_points.clone();
    }
    match style_value(style, "edgeStyle") {
        Some("orthogonalEdgeStyle") => orthogonal_centerline(source, target),
        Some("isometricEdgeStyle") => {
            let elbow = style_value(style, "elbow");
            isometric_centerline(source, target, elbow)
        }
        Some("entityRelationEdgeStyle") => {
            let segment = style_value(style, "segment")
                .and_then(|value| value.parse::<f64>().ok())
                .unwrap_or(20.0);
            entity_relation_points(edge_path, segment)
        }
        _ => vec![source, target],
    }
}

fn orthogonal_centerline(source: Point, target: Point) -> Vec<Point> {
    let dx = (target.x - source.x).abs();
    let dy = (target.y - source.y).abs();
    if dx < f64::EPSILON || dy < f64::EPSILON {
        return vec![source, target];
    }
    if dx >= dy {
        let mid_x = (source.x + target.x) / 2.0;
        vec![
            source,
            Point {
                x: mid_x,
                y: source.y,
            },
            Point {
                x: mid_x,
                y: target.y,
            },
            target,
        ]
    } else {
        let mid_y = (source.y + target.y) / 2.0;
        vec![
            source,
            Point {
                x: source.x,
                y: mid_y,
            },
            Point {
                x: target.x,
                y: mid_y,
            },
            target,
        ]
    }
}

fn isometric_centerline(source: Point, target: Point, elbow: Option<&str>) -> Vec<Point> {
    let sqrt3 = 3.0_f64.sqrt();
    let (u1, v1) = (source.x + source.y * sqrt3, source.x - source.y * sqrt3);
    let (u2, v2) = (target.x + target.y * sqrt3, target.x - target.y * sqrt3);
    let use_mid_u = matches!(elbow, Some("vertical"));
    if use_mid_u {
        let mid_u = (u1 + u2) / 2.0;
        let a = Point {
            x: (mid_u + v1) / 2.0,
            y: (mid_u - v1) / (2.0 * sqrt3),
        };
        let b = Point {
            x: (mid_u + v2) / 2.0,
            y: (mid_u - v2) / (2.0 * sqrt3),
        };
        vec![source, a, b, target]
    } else {
        let mid_v = (v1 + v2) / 2.0;
        let a = Point {
            x: (u1 + mid_v) / 2.0,
            y: (u1 - mid_v) / (2.0 * sqrt3),
        };
        let b = Point {
            x: (u2 + mid_v) / 2.0,
            y: (u2 - mid_v) / (2.0 * sqrt3),
        };
        vec![source, a, b, target]
    }
}

fn entity_relation_points(edge_path: &EdgePath, segment: f64) -> Vec<Point> {
    let curve_ratio = 0.9745;
    let curve_y_ratio = 0.316;
    let start = edge_path
        .line_points
        .first()
        .copied()
        .unwrap_or(edge_path.source_anchor);
    let end = edge_path
        .line_points
        .last()
        .copied()
        .unwrap_or(edge_path.target_anchor);
    let dir = entity_relation_dir(edge_path);
    let perp = Point {
        x: -dir.y,
        y: dir.x,
    };
    let source_segment = Point {
        x: edge_path.source_anchor.x + dir.x * segment,
        y: edge_path.source_anchor.y + dir.y * segment,
    };
    let target_segment = Point {
        x: edge_path.target_anchor.x - dir.x * segment,
        y: edge_path.target_anchor.y - dir.y * segment,
    };
    let dy = target_segment.y - source_segment.y;
    let curve_x = segment * curve_ratio;
    let curve_y = dy * curve_y_ratio;
    let curve_start = Point {
        x: source_segment.x + dir.x * curve_x + perp.x * curve_y,
        y: source_segment.y + dir.y * curve_x + perp.y * curve_y,
    };
    let curve_end = Point {
        x: target_segment.x - dir.x * curve_x - perp.x * curve_y,
        y: target_segment.y - dir.y * curve_x - perp.y * curve_y,
    };
    vec![
        start,
        source_segment,
        curve_start,
        curve_end,
        target_segment,
        end,
    ]
}

fn entity_relation_dir(edge_path: &EdgePath) -> Point {
    axis_dir_from_points(edge_path.source_anchor, edge_path.target_anchor)
}

fn axis_dir_from_points(start: Point, end: Point) -> Point {
    let dx = end.x - start.x;
    let dy = end.y - start.y;
    if dx.abs() >= dy.abs() {
        let sign = if dx >= 0.0 { 1.0 } else { -1.0 };
        Point { x: sign, y: 0.0 }
    } else {
        let sign = if dy >= 0.0 { 1.0 } else { -1.0 };
        Point { x: 0.0, y: sign }
    }
}

fn flex_arrow_outline(
    centerline: &[Point],
    start_head: bool,
    end_head: bool,
    include_end_head_points: bool,
    metrics: FlexArrowMetrics,
    use_orthogonal_offsets: bool,
) -> (Vec<Point>, Vec<bool>) {
    let mut points = centerline.to_vec();
    if points.len() < 2 {
        return (Vec::new(), Vec::new());
    }
    let start_trim = if start_head { metrics.head_length } else { 0.0 };
    let end_trim = if end_head && include_end_head_points {
        metrics.head_length
    } else {
        0.0
    };
    points = trim_polyline(&points, start_trim, end_trim);
    if points.len() < 2 {
        return (Vec::new(), Vec::new());
    }
    let (mut left, mut right) = if use_orthogonal_offsets {
        offset_polyline_orthogonal_segmented(&points, metrics.body_half)
            .unwrap_or_else(|| offset_polyline(&points, metrics.body_half))
    } else {
        offset_polyline(&points, metrics.body_half)
    };
    if left.is_empty() || right.is_empty() {
        return (Vec::new(), Vec::new());
    }
    if use_orthogonal_offsets {
        std::mem::swap(&mut left, &mut right);
    }
    let mut outline = Vec::new();
    let mut round_skip = Vec::new();
    let push_point =
        |point: Point, skip_round: bool, outline: &mut Vec<Point>, skip: &mut Vec<bool>| {
            outline.push(point);
            skip.push(skip_round);
        };
    if start_head {
        push_point(left[0], false, &mut outline, &mut round_skip);
        if let Some((tip, left_base, right_base)) =
            flex_arrow_head_points(centerline, false, metrics)
        {
            push_point(right_base, true, &mut outline, &mut round_skip);
            push_point(tip, true, &mut outline, &mut round_skip);
            push_point(left_base, true, &mut outline, &mut round_skip);
        }
        for point in &right {
            push_point(*point, false, &mut outline, &mut round_skip);
        }
        if end_head
            && include_end_head_points
            && let Some((tip, left_base, right_base)) =
                flex_arrow_head_points(centerline, true, metrics)
        {
            push_point(left_base, true, &mut outline, &mut round_skip);
            push_point(tip, true, &mut outline, &mut round_skip);
            push_point(right_base, true, &mut outline, &mut round_skip);
        }
        for point in left.iter().rev().take(left.len().saturating_sub(1)) {
            push_point(*point, false, &mut outline, &mut round_skip);
        }
    } else {
        for point in &left {
            push_point(*point, false, &mut outline, &mut round_skip);
        }
        let mut skip_first_right = false;
        if end_head
            && include_end_head_points
            && let Some((tip, left_base, right_base)) =
                flex_arrow_head_points(centerline, true, metrics)
        {
            if let Some(last) = round_skip.last_mut() {
                *last = true;
            }
            push_point(left_base, true, &mut outline, &mut round_skip);
            push_point(tip, true, &mut outline, &mut round_skip);
            push_point(right_base, true, &mut outline, &mut round_skip);
            skip_first_right = true;
        }
        for (idx, point) in right.into_iter().rev().enumerate() {
            let skip = skip_first_right && idx == 0;
            push_point(point, skip, &mut outline, &mut round_skip);
        }
    }
    if use_orthogonal_offsets && outline.len() > 2 {
        let find_start = |points: &[Point]| -> (usize, bool) {
            let min_x = points
                .iter()
                .fold(f64::INFINITY, |acc, point| acc.min(point.x));
            let mut fallback_idx = 0;
            let mut vertical_idx = None;
            for idx in 0..points.len() {
                let current = points[idx];
                if (current.x - min_x).abs() > 1e-6 {
                    continue;
                }
                let next = points[(idx + 1) % points.len()];
                if (next.x - current.x).abs() <= 1e-6 {
                    if next.y < current.y - 1e-6 {
                        return (idx, true);
                    }
                    if vertical_idx.is_none() {
                        vertical_idx = Some(idx);
                    }
                } else if vertical_idx.is_none() {
                    let best = points[fallback_idx];
                    if current.y > best.y + 1e-6 {
                        fallback_idx = idx;
                    }
                }
            }
            (vertical_idx.unwrap_or(fallback_idx), false)
        };

        let (mut start_idx, found_upward) = find_start(&outline);
        if !found_upward {
            outline.reverse();
            round_skip.reverse();
            let (rev_idx, _) = find_start(&outline);
            start_idx = rev_idx;
        }
        if start_idx != 0 {
            let mut rotated = Vec::with_capacity(outline.len());
            let mut rotated_skip = Vec::with_capacity(round_skip.len());
            for offset in 0..outline.len() {
                let idx = (start_idx + offset) % outline.len();
                rotated.push(outline[idx]);
                rotated_skip.push(round_skip[idx]);
            }
            outline = rotated;
            round_skip = rotated_skip;
        }
    }
    (outline, round_skip)
}

fn flex_arrow_head_points(
    centerline: &[Point],
    at_end: bool,
    metrics: FlexArrowMetrics,
) -> Option<(Point, Point, Point)> {
    if centerline.len() < 2 {
        return None;
    }
    let (tip, base, dir) = if at_end {
        let end = *centerline.last()?;
        let prev = centerline[centerline.len() - 2];
        let dir = normalize(Point {
            x: end.x - prev.x,
            y: end.y - prev.y,
        });
        let tip = Point {
            x: end.x - dir.x * metrics.tip_gap,
            y: end.y - dir.y * metrics.tip_gap,
        };
        let base = Point {
            x: end.x - dir.x * metrics.head_length,
            y: end.y - dir.y * metrics.head_length,
        };
        (tip, base, dir)
    } else {
        let start = centerline[0];
        let next = centerline[1];
        let dir = normalize(Point {
            x: next.x - start.x,
            y: next.y - start.y,
        });
        let tip = Point {
            x: start.x + dir.x * metrics.tip_gap,
            y: start.y + dir.y * metrics.tip_gap,
        };
        let base = Point {
            x: start.x + dir.x * metrics.head_length,
            y: start.y + dir.y * metrics.head_length,
        };
        (tip, base, dir)
    };
    let perp = Point {
        x: dir.y,
        y: -dir.x,
    };
    let left_base = Point {
        x: base.x + perp.x * metrics.head_half,
        y: base.y + perp.y * metrics.head_half,
    };
    let right_base = Point {
        x: base.x - perp.x * metrics.head_half,
        y: base.y - perp.y * metrics.head_half,
    };
    Some((tip, left_base, right_base))
}

fn flex_arrow_head_path(
    end: Point,
    prev: Point,
    metrics: FlexArrowMetrics,
    min_x: f64,
    min_y: f64,
) -> Option<String> {
    let dir = normalize(Point {
        x: end.x - prev.x,
        y: end.y - prev.y,
    });
    let tip = Point {
        x: end.x - dir.x * metrics.tip_gap,
        y: end.y - dir.y * metrics.tip_gap,
    };
    let base = Point {
        x: end.x - dir.x * metrics.head_length,
        y: end.y - dir.y * metrics.head_length,
    };
    let perp = Point {
        x: dir.y,
        y: -dir.x,
    };
    let start = Point {
        x: base.x + perp.x * metrics.body_half,
        y: base.y + perp.y * metrics.body_half,
    };
    let left = Point {
        x: base.x + perp.x * metrics.head_half,
        y: base.y + perp.y * metrics.head_half,
    };
    let right = Point {
        x: base.x - perp.x * metrics.head_half,
        y: base.y - perp.y * metrics.head_half,
    };
    let end = Point {
        x: base.x - perp.x * metrics.body_half,
        y: base.y - perp.y * metrics.body_half,
    };
    let mut d = String::new();
    write!(
        d,
        "M {} {} L {} {} L {} {} L {} {} L {} {}",
        fmt_num(start.x - min_x),
        fmt_num(start.y - min_y),
        fmt_num(left.x - min_x),
        fmt_num(left.y - min_y),
        fmt_num(tip.x - min_x),
        fmt_num(tip.y - min_y),
        fmt_num(right.x - min_x),
        fmt_num(right.y - min_y),
        fmt_num(end.x - min_x),
        fmt_num(end.y - min_y)
    )
    .unwrap();
    Some(d)
}

fn trim_polyline(points: &[Point], start_trim: f64, end_trim: f64) -> Vec<Point> {
    if points.len() < 2 {
        return points.to_vec();
    }
    let mut out = points.to_vec();
    if start_trim > 0.0 {
        out = trim_polyline_from_start(&out, start_trim);
    }
    if end_trim > 0.0 {
        out = trim_polyline_from_end(&out, end_trim);
    }
    out
}

fn trim_polyline_from_start(points: &[Point], mut trim: f64) -> Vec<Point> {
    let mut out = Vec::new();
    let mut idx = 0;
    while idx + 1 < points.len() {
        let a = points[idx];
        let b = points[idx + 1];
        let seg_len = distance(a, b);
        if seg_len <= f64::EPSILON {
            idx += 1;
            continue;
        }
        if trim < seg_len {
            let t = trim / seg_len;
            out.push(Point {
                x: a.x + (b.x - a.x) * t,
                y: a.y + (b.y - a.y) * t,
            });
            out.extend_from_slice(&points[idx + 1..]);
            return out;
        }
        trim -= seg_len;
        idx += 1;
    }
    vec![*points.last().unwrap()]
}

fn trim_polyline_from_end(points: &[Point], mut trim: f64) -> Vec<Point> {
    let mut out = Vec::new();
    let mut idx = points.len() - 1;
    while idx > 0 {
        let a = points[idx];
        let b = points[idx - 1];
        let seg_len = distance(a, b);
        if seg_len <= f64::EPSILON {
            idx -= 1;
            continue;
        }
        if trim < seg_len {
            let t = trim / seg_len;
            let point = Point {
                x: a.x + (b.x - a.x) * t,
                y: a.y + (b.y - a.y) * t,
            };
            out.extend_from_slice(&points[..idx]);
            out.push(point);
            return out;
        }
        trim -= seg_len;
        idx -= 1;
    }
    vec![points[0]]
}

fn offset_polyline(points: &[Point], half: f64) -> (Vec<Point>, Vec<Point>) {
    let mut left = Vec::with_capacity(points.len());
    let mut right = Vec::with_capacity(points.len());
    for (idx, point) in points.iter().enumerate() {
        let (dir_prev, dir_next) = if idx == 0 {
            let dir = normalize(Point {
                x: points[1].x - point.x,
                y: points[1].y - point.y,
            });
            (dir, dir)
        } else if idx + 1 == points.len() {
            let dir = normalize(Point {
                x: point.x - points[idx - 1].x,
                y: point.y - points[idx - 1].y,
            });
            (dir, dir)
        } else {
            (
                normalize(Point {
                    x: point.x - points[idx - 1].x,
                    y: point.y - points[idx - 1].y,
                }),
                normalize(Point {
                    x: points[idx + 1].x - point.x,
                    y: points[idx + 1].y - point.y,
                }),
            )
        };
        let perp_prev = Point {
            x: -dir_prev.y,
            y: dir_prev.x,
        };
        let perp_next = Point {
            x: -dir_next.y,
            y: dir_next.x,
        };
        let mut miter = Point {
            x: perp_prev.x + perp_next.x,
            y: perp_prev.y + perp_next.y,
        };
        if miter.x.abs() < f64::EPSILON && miter.y.abs() < f64::EPSILON {
            miter = perp_prev;
        }
        let miter = normalize(miter);
        let denom = miter.x * perp_prev.x + miter.y * perp_prev.y;
        let mut miter_len = if denom.abs() > f64::EPSILON {
            half / denom
        } else {
            half
        };
        let max_miter = half * 4.0;
        if miter_len.abs() > max_miter {
            miter_len = max_miter * miter_len.signum();
        }
        left.push(Point {
            x: point.x + miter.x * miter_len,
            y: point.y + miter.y * miter_len,
        });
        right.push(Point {
            x: point.x - miter.x * miter_len,
            y: point.y - miter.y * miter_len,
        });
    }
    (left, right)
}

fn offset_polyline_orthogonal_segmented(
    points: &[Point],
    half: f64,
) -> Option<(Vec<Point>, Vec<Point>)> {
    const ORTHO_EPS: f64 = 1e-6;
    if points.len() < 2 {
        return None;
    }
    let mut dirs: Vec<Point> = Vec::with_capacity(points.len() - 1);
    for pair in points.windows(2) {
        let dx = pair[1].x - pair[0].x;
        let dy = pair[1].y - pair[0].y;
        if dx.abs() > ORTHO_EPS && dy.abs() > ORTHO_EPS {
            return None;
        }
        dirs.push(orthogonal_dir(pair[0], pair[1]));
    }
    if dirs
        .iter()
        .all(|dir| dir.x.abs() <= ORTHO_EPS && dir.y.abs() <= ORTHO_EPS)
    {
        return None;
    }

    let mut left = Vec::with_capacity(points.len());
    let mut right = Vec::with_capacity(points.len());

    let first_dir = dirs[0];
    let first_perp = Point {
        x: -first_dir.y,
        y: first_dir.x,
    };
    left.push(Point {
        x: points[0].x + first_perp.x * half,
        y: points[0].y + first_perp.y * half,
    });
    right.push(Point {
        x: points[0].x - first_perp.x * half,
        y: points[0].y - first_perp.y * half,
    });

    for idx in 1..points.len() - 1 {
        let prev_dir = dirs[idx - 1];
        let next_dir = dirs[idx];
        let prev_perp = Point {
            x: -prev_dir.y,
            y: prev_dir.x,
        };
        let next_perp = Point {
            x: -next_dir.y,
            y: next_dir.x,
        };
        let prev_left = Point {
            x: points[idx].x + prev_perp.x * half,
            y: points[idx].y + prev_perp.y * half,
        };
        let next_left = Point {
            x: points[idx].x + next_perp.x * half,
            y: points[idx].y + next_perp.y * half,
        };
        let prev_right = Point {
            x: points[idx].x - prev_perp.x * half,
            y: points[idx].y - prev_perp.y * half,
        };
        let next_right = Point {
            x: points[idx].x - next_perp.x * half,
            y: points[idx].y - next_perp.y * half,
        };
        let left_corner = if prev_dir.x.abs() > ORTHO_EPS && next_dir.y.abs() > ORTHO_EPS {
            Point {
                x: next_left.x,
                y: prev_left.y,
            }
        } else if prev_dir.y.abs() > ORTHO_EPS && next_dir.x.abs() > ORTHO_EPS {
            Point {
                x: prev_left.x,
                y: next_left.y,
            }
        } else {
            next_left
        };
        let right_corner = if prev_dir.x.abs() > ORTHO_EPS && next_dir.y.abs() > ORTHO_EPS {
            Point {
                x: next_right.x,
                y: prev_right.y,
            }
        } else if prev_dir.y.abs() > ORTHO_EPS && next_dir.x.abs() > ORTHO_EPS {
            Point {
                x: prev_right.x,
                y: next_right.y,
            }
        } else {
            next_right
        };
        left.push(left_corner);
        right.push(right_corner);
    }

    let last_dir = *dirs.last().unwrap();
    let last_perp = Point {
        x: -last_dir.y,
        y: last_dir.x,
    };
    left.push(Point {
        x: points[points.len() - 1].x + last_perp.x * half,
        y: points[points.len() - 1].y + last_perp.y * half,
    });
    right.push(Point {
        x: points[points.len() - 1].x - last_perp.x * half,
        y: points[points.len() - 1].y - last_perp.y * half,
    });

    Some((left, right))
}

fn orthogonal_dir(a: Point, b: Point) -> Point {
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    if dx.abs() > dy.abs() {
        Point {
            x: dx.signum(),
            y: 0.0,
        }
    } else {
        Point {
            x: 0.0,
            y: dy.signum(),
        }
    }
}

fn polygon_path(points: &[Point], min_x: f64, min_y: f64) -> String {
    let mut d = String::new();
    if points.is_empty() {
        return d;
    }
    write!(
        d,
        "M {} {}",
        fmt_num(points[0].x - min_x),
        fmt_num(points[0].y - min_y)
    )
    .unwrap();
    for point in &points[1..] {
        write!(
            d,
            " L {} {}",
            fmt_num(point.x - min_x),
            fmt_num(point.y - min_y)
        )
        .unwrap();
    }
    d.push_str(" Z");
    d
}

fn orthogonal_flex_round_indices(centerline: &[Point], outline: &[Point], half: f64) -> Vec<usize> {
    if centerline.len() < 3 || outline.is_empty() {
        return Vec::new();
    }
    let mut corners = Vec::new();
    for idx in 1..centerline.len() - 1 {
        let dir1 = orthogonal_dir(centerline[idx - 1], centerline[idx]);
        let dir2 = orthogonal_dir(centerline[idx], centerline[idx + 1]);
        let cross = dir1.x * dir2.y - dir1.y * dir2.x;
        if cross.abs() <= f64::EPSILON {
            continue;
        }
        let (n1, n2) = if cross > 0.0 {
            (right_normal(dir1), right_normal(dir2))
        } else {
            (left_normal(dir1), left_normal(dir2))
        };
        let corner = Point {
            x: centerline[idx].x + (n1.x + n2.x) * half,
            y: centerline[idx].y + (n1.y + n2.y) * half,
        };
        corners.push(corner);
    }

    let mut indices = Vec::new();
    for corner in corners {
        if let Some(idx) = find_outline_index(outline, corner, 1e-3)
            && !indices.contains(&idx)
        {
            indices.push(idx);
        }
    }
    indices.sort_unstable();
    indices
}

fn find_outline_index(points: &[Point], target: Point, eps: f64) -> Option<usize> {
    let mut best = None;
    let mut best_dist = f64::INFINITY;
    for (idx, point) in points.iter().enumerate() {
        let dx = point.x - target.x;
        let dy = point.y - target.y;
        let dist = dx * dx + dy * dy;
        if dist < best_dist {
            best_dist = dist;
            best = Some(idx);
        }
    }
    if best_dist <= eps * eps { best } else { None }
}

fn right_normal(dir: Point) -> Point {
    Point {
        x: dir.y,
        y: -dir.x,
    }
}

fn left_normal(dir: Point) -> Point {
    Point {
        x: -dir.y,
        y: dir.x,
    }
}

fn adjust_outline_min_x(points: &[Point], delta: f64) -> Vec<Point> {
    if points.is_empty() {
        return Vec::new();
    }
    let min_x = points
        .iter()
        .fold(f64::INFINITY, |acc, point| acc.min(point.x));
    let mut out = Vec::with_capacity(points.len());
    for point in points {
        let mut adjusted = *point;
        if (point.x - min_x).abs() <= 1e-6 {
            adjusted.x += delta;
        }
        out.push(adjusted);
    }
    out
}

#[cfg(feature = "edge-debug")]
fn polygon_area(points: &[Point]) -> f64 {
    if points.len() < 3 {
        return 0.0;
    }
    let mut area = 0.0;
    for idx in 0..points.len() {
        let current = points[idx];
        let next = points[(idx + 1) % points.len()];
        area += current.x * next.y - next.x * current.y;
    }
    area * 0.5
}

fn rounded_polygon_path_indices(
    points: &[Point],
    round_indices: &[usize],
    radius: f64,
    min_x: f64,
    min_y: f64,
) -> String {
    if points.len() < 3 || radius <= f64::EPSILON {
        return polygon_path(points, min_x, min_y);
    }
    let mut d = String::new();
    let mut current = points[0];
    write!(
        d,
        "M {} {}",
        fmt_num(current.x - min_x),
        fmt_num(current.y - min_y)
    )
    .unwrap();
    let count = points.len();
    for idx in 0..count {
        let prev = points[(idx + count - 1) % count];
        let corner = points[idx];
        let next = points[(idx + 1) % count];
        let in_vec = Point {
            x: prev.x - corner.x,
            y: prev.y - corner.y,
        };
        let out_vec = Point {
            x: next.x - corner.x,
            y: next.y - corner.y,
        };
        let in_len = (in_vec.x * in_vec.x + in_vec.y * in_vec.y).sqrt();
        let out_len = (out_vec.x * out_vec.x + out_vec.y * out_vec.y).sqrt();
        if in_len <= f64::EPSILON || out_len <= f64::EPSILON {
            continue;
        }
        if !round_indices.contains(&idx) {
            if (current.x - corner.x).abs() > f64::EPSILON
                || (current.y - corner.y).abs() > f64::EPSILON
            {
                write!(
                    d,
                    " L {} {}",
                    fmt_num(corner.x - min_x),
                    fmt_num(corner.y - min_y)
                )
                .unwrap();
                current = corner;
            }
            continue;
        }
        let r = radius.min(in_len / 2.0).min(out_len / 2.0);
        if r <= f64::EPSILON {
            if (current.x - corner.x).abs() > f64::EPSILON
                || (current.y - corner.y).abs() > f64::EPSILON
            {
                write!(
                    d,
                    " L {} {}",
                    fmt_num(corner.x - min_x),
                    fmt_num(corner.y - min_y)
                )
                .unwrap();
                current = corner;
            }
            continue;
        }
        let in_unit = Point {
            x: in_vec.x / in_len,
            y: in_vec.y / in_len,
        };
        let out_unit = Point {
            x: out_vec.x / out_len,
            y: out_vec.y / out_len,
        };
        let pre = Point {
            x: corner.x + in_unit.x * r,
            y: corner.y + in_unit.y * r,
        };
        let post = Point {
            x: corner.x + out_unit.x * r,
            y: corner.y + out_unit.y * r,
        };
        if (current.x - pre.x).abs() > f64::EPSILON || (current.y - pre.y).abs() > f64::EPSILON {
            write!(
                d,
                " L {} {}",
                fmt_num(pre.x - min_x),
                fmt_num(pre.y - min_y)
            )
            .unwrap();
        }
        write!(
            d,
            " Q {} {} {} {}",
            fmt_num(corner.x - min_x),
            fmt_num(corner.y - min_y),
            fmt_num(post.x - min_x),
            fmt_num(post.y - min_y)
        )
        .unwrap();
        current = post;
    }
    d.push_str(" Z");
    d
}

fn rounded_polygon_path(points: &[Point], radius: f64, min_x: f64, min_y: f64) -> String {
    if points.len() < 3 || radius <= f64::EPSILON {
        return polygon_path(points, min_x, min_y);
    }
    let mut d = String::new();
    let mut pts = points.to_vec();
    pts.push(points[0]);
    pts.push(points[1]);
    let start = pts[0];
    write!(
        d,
        "M {} {}",
        fmt_num(start.x - min_x),
        fmt_num(start.y - min_y)
    )
    .unwrap();
    let mut current = start;
    for window in pts.windows(3) {
        let prev = window[0];
        let corner = window[1];
        let next = window[2];
        let in_vec = Point {
            x: prev.x - corner.x,
            y: prev.y - corner.y,
        };
        let out_vec = Point {
            x: next.x - corner.x,
            y: next.y - corner.y,
        };
        let in_len = (in_vec.x * in_vec.x + in_vec.y * in_vec.y).sqrt();
        let out_len = (out_vec.x * out_vec.x + out_vec.y * out_vec.y).sqrt();
        if in_len <= f64::EPSILON || out_len <= f64::EPSILON {
            continue;
        }
        let r = radius.min(in_len / 2.0).min(out_len / 2.0);
        if r <= f64::EPSILON {
            continue;
        }
        let in_unit = Point {
            x: in_vec.x / in_len,
            y: in_vec.y / in_len,
        };
        let out_unit = Point {
            x: out_vec.x / out_len,
            y: out_vec.y / out_len,
        };
        let pre = Point {
            x: corner.x + in_unit.x * r,
            y: corner.y + in_unit.y * r,
        };
        let post = Point {
            x: corner.x + out_unit.x * r,
            y: corner.y + out_unit.y * r,
        };
        if (current.x - pre.x).abs() > f64::EPSILON || (current.y - pre.y).abs() > f64::EPSILON {
            write!(
                d,
                " L {} {}",
                fmt_num(pre.x - min_x),
                fmt_num(pre.y - min_y)
            )
            .unwrap();
        }
        write!(
            d,
            " Q {} {} {} {}",
            fmt_num(corner.x - min_x),
            fmt_num(corner.y - min_y),
            fmt_num(post.x - min_x),
            fmt_num(post.y - min_y)
        )
        .unwrap();
        current = post;
    }
    d.push_str(" Z");
    d
}

fn link_shape_path(edge_path: &EdgePath, min_x: f64, min_y: f64) -> Option<EdgeLineSegment> {
    let start = edge_path.source_anchor;
    let end = edge_path.target_anchor;
    let dir = normalize(Point {
        x: end.x - start.x,
        y: end.y - start.y,
    });
    if dir.x == 0.0 && dir.y == 0.0 {
        return None;
    }
    let perp = Point {
        x: -dir.y,
        y: dir.x,
    };
    let offset = 2.0;
    let start_neg = Point {
        x: start.x - perp.x * offset,
        y: start.y - perp.y * offset,
    };
    let end_neg = Point {
        x: end.x - perp.x * offset,
        y: end.y - perp.y * offset,
    };
    let start_pos = Point {
        x: start.x + perp.x * offset,
        y: start.y + perp.y * offset,
    };
    let end_pos = Point {
        x: end.x + perp.x * offset,
        y: end.y + perp.y * offset,
    };
    let mut d = String::new();
    write!(
        d,
        "M {} {} L {} {} M {} {} L {} {} M {} {}",
        fmt_num(start_neg.x - min_x),
        fmt_num(start_neg.y - min_y),
        fmt_num(end_neg.x - min_x),
        fmt_num(end_neg.y - min_y),
        fmt_num(end_pos.x - min_x),
        fmt_num(end_pos.y - min_y),
        fmt_num(start_pos.x - min_x),
        fmt_num(start_pos.y - min_y),
        fmt_num(end_pos.x - min_x),
        fmt_num(end_pos.y - min_y)
    )
    .unwrap();
    Some(EdgeLineSegment {
        d,
        fill: None,
        linejoin: Some("round"),
        miterlimit: None,
        pointer_events: "all",
    })
}

fn vertex_top_left(cell: &MxCell, cell_by_id: &BTreeMap<String, &MxCell>) -> SvgResult<Point> {
    let geometry = cell
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
    let width = geometry.width.unwrap_or(0.0);
    let height = geometry.height.unwrap_or(0.0);
    if geometry.relative == Some(true)
        && let Some(parent_id) = cell.parent.as_ref()
        && let Some(parent) = cell_by_id.get(parent_id)
        && parent.edge == Some(true)
        && let Some(edge_path) = edge_path_absolute(parent, cell_by_id)?
    {
        let rotation = rotation_degrees(cell.style.as_deref());
        if let Some(center) =
            edge_label_position(parent, &edge_path, cell_by_id, geometry, rotation)
        {
            let offset = geometry
                .offset_point
                .as_ref()
                .and_then(point_from_mxpoint)
                .unwrap_or(Point {
                    x: -width / 2.0,
                    y: -height / 2.0,
                });
            return Ok(Point {
                x: center.x + offset.x,
                y: center.y + offset.y,
            });
        }
    }
    let offset = parent_offset(cell, cell_by_id);
    Ok(Point {
        x: geometry.x.unwrap_or(0.0) + offset.x,
        y: geometry.y.unwrap_or(0.0) + offset.y,
    })
}

fn vertex_top_left_raw(cell: &MxCell, cell_by_id: &BTreeMap<String, &MxCell>) -> SvgResult<Point> {
    let geometry = cell
        .geometry
        .as_ref()
        .ok_or_else(|| SvgError::MissingGeometry(cell.id.clone()))?;
    let offset = parent_offset_raw(cell, cell_by_id);
    Ok(Point {
        x: geometry.raw_x.or(geometry.x).unwrap_or(0.0) + offset.x,
        y: geometry.raw_y.or(geometry.y).unwrap_or(0.0) + offset.y,
    })
}

fn parent_offset(cell: &MxCell, cell_by_id: &BTreeMap<String, &MxCell>) -> Point {
    let mut offset = Point { x: 0.0, y: 0.0 };
    let mut current = cell;
    while let Some(parent_id) = current.parent.as_ref() {
        let Some(parent) = cell_by_id.get(parent_id) else {
            break;
        };
        if parent.vertex == Some(true)
            && parent.edge != Some(true)
            && let Some(geometry) = parent.geometry.as_ref()
        {
            offset.x += geometry.x.unwrap_or(0.0);
            offset.y += geometry.y.unwrap_or(0.0);
        }
        current = parent;
    }
    offset
}

fn parent_offset_raw(cell: &MxCell, cell_by_id: &BTreeMap<String, &MxCell>) -> Point {
    let mut offset = Point { x: 0.0, y: 0.0 };
    let mut current = cell;
    while let Some(parent_id) = current.parent.as_ref() {
        let Some(parent) = cell_by_id.get(parent_id) else {
            break;
        };
        if parent.vertex == Some(true)
            && parent.edge != Some(true)
            && let Some(geometry) = parent.geometry.as_ref()
        {
            offset.x += geometry.raw_x.or(geometry.x).unwrap_or(0.0);
            offset.y += geometry.raw_y.or(geometry.y).unwrap_or(0.0);
        }
        current = parent;
    }
    offset
}

fn is_ancestor(ancestor: &MxCell, node: &MxCell, cell_by_id: &BTreeMap<String, &MxCell>) -> bool {
    let mut current = node;
    while let Some(parent_id) = current.parent.as_ref() {
        if parent_id == &ancestor.id {
            return true;
        }
        let Some(parent) = cell_by_id.get(parent_id).copied() else {
            break;
        };
        current = parent;
    }
    false
}

fn rect_intersection(center: Point, half_w: f64, half_h: f64, rotation: f64, dir: Point) -> Point {
    let angle = rotation.to_radians();
    let dir = normalize(dir);
    let local_dx = dir.x * angle.cos() + dir.y * angle.sin();
    let local_dy = -dir.x * angle.sin() + dir.y * angle.cos();
    let tx = if local_dx.abs() > f64::EPSILON {
        half_w / local_dx.abs()
    } else {
        f64::INFINITY
    };
    let ty = if local_dy.abs() > f64::EPSILON {
        half_h / local_dy.abs()
    } else {
        f64::INFINITY
    };
    let t = tx.min(ty);
    let local_x = local_dx * t;
    let local_y = local_dy * t;
    let global_x = local_x * angle.cos() - local_y * angle.sin();
    let global_y = local_x * angle.sin() + local_y * angle.cos();
    Point {
        x: center.x + global_x,
        y: center.y + global_y,
    }
}

fn ellipse_intersection(
    center: Point,
    radius_x: f64,
    radius_y: f64,
    rotation: f64,
    dir: Point,
) -> Point {
    let angle = rotation.to_radians();
    let dir = normalize(dir);
    let local_dx = dir.x * angle.cos() + dir.y * angle.sin();
    let local_dy = -dir.x * angle.sin() + dir.y * angle.cos();
    let denom = (local_dx * local_dx) / (radius_x * radius_x)
        + (local_dy * local_dy) / (radius_y * radius_y);
    let t = if denom == 0.0 {
        0.0
    } else {
        1.0 / denom.sqrt()
    };
    let local_x = local_dx * t;
    let local_y = local_dy * t;
    let global_x = local_x * angle.cos() - local_y * angle.sin();
    let global_y = local_x * angle.sin() + local_y * angle.cos();
    Point {
        x: center.x + global_x,
        y: center.y + global_y,
    }
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

fn fmt_num_raw(value: f64) -> String {
    format!("{value}")
}

fn snap_coord(value: f64) -> f64 {
    let rounded = value.round();
    if (value - rounded).abs() < 1e-9 {
        rounded
    } else {
        value
    }
}

fn render_vertex_label(vertex: &MxCell, x: f64, y: f64, width: f64, height: f64) -> Option<String> {
    let value = vertex.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let style = vertex.style.as_deref();
    let vertical_label_bottom = style_value(style, "verticalLabelPosition") == Some("bottom");
    let vertical_text = style_value(style, "horizontal") == Some("0");
    let spacing = style_value(style, "spacing")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(0.0);
    let spacing_left = style_value(style, "spacingLeft")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(spacing / 2.0);
    let spacing_right = style_value(style, "spacingRight")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(spacing / 2.0);
    let label_width = if vertical_label_bottom {
        1.0
    } else if vertical_text {
        (height - 2.0 - spacing_left - spacing_right).max(0.0)
    } else {
        (width - 2.0 - spacing_left - spacing_right).max(0.0)
    };
    let (justify_content, text_align, margin_offset) = if vertical_label_bottom {
        ("center", "center", 0.0)
    } else {
        label_alignment(style)
    };
    let padding_top = if vertical_label_bottom {
        y + height + 7.0
    } else {
        label_padding_top(style, y, height)
    };
    let margin_left = if vertical_label_bottom {
        x + width / 2.0
    } else if vertical_text {
        x + margin_offset + spacing_left + (width - height) / 2.0
    } else {
        x + margin_offset + spacing_left
    };
    let bold = is_bold(style);
    let text = label_text(value, style);
    let (text_color, inner_color) = text_colors(style);
    let rotation = label_rotation_degrees(style);
    let overflow_hidden = style_value(style, "overflow") == Some("hidden");
    let overflow_style = if overflow_hidden {
        let max_height = (height - 4.0).max(0.0);
        format!(" max-height: {}px; overflow: hidden; ", fmt_num(max_height))
    } else {
        " ".to_string()
    };
    let white_space_style = if style_value(style, "whiteSpace") == Some("wrap") {
        "white-space: normal; word-wrap: normal; ".to_string()
    } else {
        "white-space: nowrap; ".to_string()
    };
    let (open, close) = if rotation.abs() > f64::EPSILON {
        let center_x = x + width / 2.0;
        let center_y = y + height / 2.0;
        (
            format!(
                "<g><g transform=\"rotate({} {} {})\">",
                fmt_num_raw(rotation),
                fmt_num_raw(center_x),
                fmt_num_raw(center_y)
            ),
            "</g></g>".to_string(),
        )
    } else {
        ("<g><g>".to_string(), "</g></g>".to_string())
    };

    let mut out = String::new();
    out.push_str(&open);
    write!(
        out,
        "<foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe {}; justify-content: unsafe {}; width: {}px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: {};{}color: {}; \"><div style=\"display: inline-block; font-size: 12px; font-family: Helvetica; color: {}; line-height: 1.2; pointer-events: all; {}{}\">{}</div></div></div></foreignObject>",
        label_align_items(style),
        justify_content,
        fmt_num(label_width),
        fmt_num(padding_top),
        fmt_num(margin_left),
        text_align,
        overflow_style,
        text_color,
        inner_color,
        bold_style(bold),
        white_space_style,
        text
    )
    .unwrap();
    out.push_str(&close);
    Some(out)
}

fn swimlane_start_size(style: Option<&str>) -> f64 {
    style_value(style, "startSize")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(12.0 + 11.0)
}

fn swimlane_title_bold(style: Option<&str>) -> bool {
    if style_value(style, "fontStyle").is_some() {
        is_bold(style)
    } else {
        true
    }
}

fn swimlane_is_vertical(cell: &MxCell) -> bool {
    let style = cell.style.as_deref();
    style_value(style, "horizontal") == Some("0") || is_collapsed(cell)
}

#[allow(clippy::too_many_arguments)]
fn render_swimlane_label(
    vertex: &MxCell,
    x: f64,
    y: f64,
    raw_x: f64,
    raw_y: f64,
    width: f64,
    height: f64,
    start_size: f64,
    vertical: bool,
) -> Option<(String, bool)> {
    let value = vertex.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let style = vertex.style.as_deref();
    let text = label_text(value, style);
    let bold = swimlane_title_bold(style);
    let (text_color, inner_color) = text_colors(style);
    let text_style = if style_value(style, "fontColor").is_some() {
        inner_color.clone()
    } else {
        "light-dark(rgb(0, 0, 0), rgb(255, 255, 255))".to_string()
    };
    let wrap_label = style_value(style, "whiteSpace") == Some("wrap");
    let (label_width, padding_top, margin_left, open, close, white_space_style) = if vertical {
        let center_x = raw_x + start_size / 2.0;
        let center_y = raw_y + height / 2.0;
        (
            1.0,
            center_y,
            center_x,
            format!(
                "<g><g transform=\"rotate(-90 {} {})\">",
                fmt_num_raw(center_x),
                fmt_num_raw(center_y)
            ),
            "</g></g>".to_string(),
            "white-space: nowrap; ".to_string(),
        )
    } else if wrap_label {
        (
            (width - 2.0).max(0.0),
            y + (start_size / 2.0).ceil(),
            x + 1.0,
            "<g><g>".to_string(),
            "</g></g>".to_string(),
            "white-space: normal; word-wrap: normal; ".to_string(),
        )
    } else {
        (
            1.0,
            y + (start_size / 2.0).ceil(),
            x + width / 2.0,
            "<g><g>".to_string(),
            "</g></g>".to_string(),
            "white-space: nowrap; ".to_string(),
        )
    };
    if style_value(style, "html") == Some("1") {
        let mut out = String::new();
        write!(
            out,
            "{}<foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe center; justify-content: unsafe center; width: {}px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: center; color: {}; \"><div style=\"display: inline-block; font-size: 12px; font-family: Helvetica; color: {}; line-height: 1.2; pointer-events: all; {}{}\">{}</div></div></div></foreignObject>{}",
            open,
            fmt_num(label_width),
            fmt_num(padding_top),
            fmt_num(margin_left),
            text_color,
            inner_color,
            bold_style(bold),
            white_space_style,
            text,
            close
        )
        .unwrap();
        return Some((out, true));
    }
    let (text_x, text_y, transform_attr) = if vertical {
        let center_x = raw_x + start_size / 2.0;
        let center_y = raw_y + height / 2.0;
        (
            center_x,
            center_y + 5.0,
            format!(
                " transform=\"rotate(-90,{},{})\"",
                fmt_num_raw(center_x),
                fmt_num_raw(center_y)
            ),
        )
    } else {
        (x + width / 2.0, y + start_size - 5.0, String::new())
    };
    let font_weight = if bold { " font-weight=\"bold\"" } else { "" };
    let out = format!(
        "<g><g fill=\"{}\" font-family=\"Helvetica\" font-size=\"12px\"{} style=\"fill: {};\" text-anchor=\"middle\"{}><text x=\"{}\" y=\"{}\">{}</text></g></g>",
        text_color,
        font_weight,
        text_style,
        transform_attr,
        fmt_num(text_x),
        fmt_num(text_y),
        text
    );
    Some((out, false))
}

fn render_edge_value_label(
    edge: &MxCell,
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
) -> Option<String> {
    let value = edge.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let mut label_points = edge_path.full_points.clone();
    if let Some(first) = label_points.first_mut() {
        *first = edge_path.source_anchor;
    }
    if let Some(last) = label_points.last_mut() {
        *last = edge_path.target_anchor;
    }
    let (center, _) = point_along_polyline(&label_points, 0.5)?;
    render_edge_label_at(edge, center.x - min_x, center.y - min_y, None)
}

fn render_edge_label_cell(
    edge: &MxCell,
    label: &MxCell,
    cell_by_id: &BTreeMap<String, &MxCell>,
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
) -> Option<String> {
    let value = label.value.as_deref()?.trim();
    if value.is_empty() {
        return None;
    }
    let rotation = rotation_degrees(label.style.as_deref());
    let geom = label.geometry.as_ref()?;
    let position = edge_label_position(edge, edge_path, cell_by_id, geom, rotation)?;
    let rendered = render_edge_label_at(
        label,
        position.x - min_x,
        position.y - min_y,
        Some(rotation),
    )?;
    Some(format!("<g data-cell-id=\"{}\">{}</g>", label.id, rendered))
}

fn edge_label_position(
    edge: &MxCell,
    edge_path: &EdgePath,
    cell_by_id: &BTreeMap<String, &MxCell>,
    geom: &MxGeometry,
    rotation: f64,
) -> Option<Point> {
    let (center, direction) = point_along_polyline(&edge_path.line_points, 0.5)?;
    let line_length = polyline_length(&edge_path.line_points);
    let y_offset = geom.y.unwrap_or(0.0);
    let x_offset = geom.x.unwrap_or(0.0);
    let has_rotation = rotation.abs() > f64::EPSILON;
    let is_vertical = direction.y.abs() >= direction.x.abs();
    // Draw.io rounds geometry.x-based offsets to whole pixels, then adds a
    // +1px bias plus the midpoint shift caused by arrow trimming.
    let round_offset = (x_offset * line_length / 2.0).round();
    let base_parallel = 1.0 + (edge_path.end_offset - edge_path.start_offset) / 2.0;
    let mut parallel_offset = base_parallel + round_offset;
    let perp_offset = if is_vertical {
        if has_rotation {
            let max_terminal_rotation = edge
                .source
                .as_ref()
                .and_then(|id| cell_by_id.get(id))
                .map(|cell| rotation_degrees(cell.style.as_deref()).abs())
                .unwrap_or(0.0)
                .max(
                    edge.target
                        .as_ref()
                        .and_then(|id| cell_by_id.get(id))
                        .map(|cell| rotation_degrees(cell.style.as_deref()).abs())
                        .unwrap_or(0.0),
                );
            let rotation_factor = (1.0 - max_terminal_rotation / 75.0
                + max_terminal_rotation / 15000.0
                + max_terminal_rotation / 12_989_318.0)
                .clamp(0.0, 1.0);
            parallel_offset -= (1.0 - rotation.to_radians().cos()) * rotation_factor;
            y_offset
        } else {
            1.0 + y_offset
        }
    } else if has_rotation {
        parallel_offset -= 1.0;
        1.0 - y_offset - (1.0 - rotation.to_radians().cos())
    } else {
        1.0 - y_offset
    };
    let perp = if is_vertical {
        Point {
            x: direction.y,
            y: -direction.x,
        }
    } else {
        Point {
            x: -direction.y,
            y: direction.x,
        }
    };
    Some(Point {
        x: center.x + direction.x * parallel_offset + perp.x * perp_offset,
        y: center.y + direction.y * parallel_offset + perp.y * perp_offset,
    })
}

fn render_edge_label_at(
    label_cell: &MxCell,
    center_x: f64,
    center_y: f64,
    rotation: Option<f64>,
) -> Option<String> {
    let style = label_cell.style.as_deref();
    let bold = is_bold(style);
    let text = label_text(label_cell.value.as_deref()?.trim(), style);
    let (text_color, inner_color) = text_colors(style);
    let padding_top = center_y.round();
    let margin_left = center_x.round();
    let (open, close) = if let Some(angle) = rotation
        && angle.abs() > f64::EPSILON
    {
        (
            format!(
                "<g><g transform=\"rotate({} {} {})\">",
                fmt_num_raw(angle),
                fmt_num_raw(center_x),
                fmt_num_raw(center_y)
            ),
            "</g></g>".to_string(),
        )
    } else {
        ("<g><g>".to_string(), "</g></g>".to_string())
    };

    let mut out = String::new();
    out.push_str(&open);
    write!(
        out,
        "<foreignObject style=\"overflow: visible; text-align: left;\" pointer-events=\"none\" width=\"100%\" height=\"100%\" requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"><div xmlns=\"http://www.w3.org/1999/xhtml\" style=\"display: flex; align-items: unsafe center; justify-content: unsafe center; width: 1px; height: 1px; padding-top: {}px; margin-left: {}px;\"><div style=\"box-sizing: border-box; font-size: 0; text-align: center; color: {}; background-color: #ffffff; \"><div style=\"display: inline-block; font-size: 11px; font-family: Helvetica; color: {}; line-height: 1.2; pointer-events: all; background-color: light-dark(#ffffff, var(--ge-dark-color, #121212)); {}white-space: nowrap; \">{}</div></div></div></foreignObject>",
        fmt_num(padding_top),
        fmt_num(margin_left),
        text_color,
        inner_color,
        bold_style(bold),
        text
    )
    .unwrap();
    out.push_str(&close);
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

fn is_uml_actor(style: Option<&str>) -> bool {
    style_value(style, "shape") == Some("umlActor")
}

fn is_swimlane(style: Option<&str>) -> bool {
    style_has_flag(style, "swimlane")
}

fn is_partial_rectangle(style: Option<&str>) -> bool {
    style_value(style, "shape") == Some("partialRectangle")
}

fn is_edge_label(style: Option<&str>) -> bool {
    style
        .unwrap_or("")
        .split(';')
        .any(|entry| entry == "edgeLabel")
}

fn perimeter_spacing(style: Option<&str>) -> f64 {
    style_value(style, "perimeterSpacing")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(0.0)
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

#[allow(clippy::too_many_arguments)]
fn expand_marker_bounds(
    min_x: &mut f64,
    min_y: &mut f64,
    max_x: &mut f64,
    max_y: &mut f64,
    anchor: Point,
    dir: Point,
    kind: MarkerKind,
    stroke_width: f64,
    metrics: (f64, f64, f64, f64),
) {
    let (local_min_x, local_min_y, local_max_x, local_max_y) =
        marker_local_bounds(kind, stroke_width, metrics);
    let perp = Point {
        x: -dir.y,
        y: dir.x,
    };
    let to_world = |p: Point| Point {
        x: anchor.x + dir.x * p.x + perp.x * p.y,
        y: anchor.y + dir.y * p.x + perp.y * p.y,
    };
    let corners = [
        Point {
            x: local_min_x,
            y: local_min_y,
        },
        Point {
            x: local_min_x,
            y: local_max_y,
        },
        Point {
            x: local_max_x,
            y: local_min_y,
        },
        Point {
            x: local_max_x,
            y: local_max_y,
        },
    ];
    for corner in corners {
        let point = to_world(corner);
        *min_x = min_x.min(point.x);
        *min_y = min_y.min(point.y);
        *max_x = max_x.max(point.x);
        *max_y = max_y.max(point.y);
    }
}

fn marker_local_bounds(
    kind: MarkerKind,
    stroke_width: f64,
    metrics: (f64, f64, f64, f64),
) -> (f64, f64, f64, f64) {
    let (tip_gap, arrow_length, arrow_back, arrow_half_height) = metrics;
    match kind {
        MarkerKind::Classic | MarkerKind::ClassicThin => {
            let half_height = if kind == MarkerKind::ClassicThin {
                arrow_half_height * (2.0 / 3.0)
            } else {
                arrow_half_height
            };
            (
                tip_gap,
                -half_height,
                tip_gap + arrow_length + arrow_back,
                half_height,
            )
        }
        MarkerKind::Open | MarkerKind::OpenThin => {
            let half_height = if kind == MarkerKind::OpenThin {
                arrow_half_height * (2.0 / 3.0)
            } else {
                arrow_half_height
            };
            (
                tip_gap,
                -half_height,
                tip_gap + arrow_length + arrow_back,
                half_height,
            )
        }
        MarkerKind::OpenAsync => (
            0.0,
            -arrow_half_height,
            arrow_length + arrow_back,
            arrow_half_height,
        ),
        MarkerKind::Oval => {
            let radius = 3.0 * stroke_width;
            (-radius, -radius, radius, radius)
        }
        MarkerKind::Diamond | MarkerKind::DiamondThin => {
            let half_width = 3.5;
            let (half_height, local_tip_gap) = if kind == MarkerKind::DiamondThin {
                (2.06, 0.99)
            } else {
                (3.5, 0.71)
            };
            (
                local_tip_gap,
                -half_height,
                local_tip_gap + 2.0 * half_width,
                half_height,
            )
        }
        MarkerKind::Block | MarkerKind::BlockThin => {
            let half_height = if kind == MarkerKind::BlockThin {
                arrow_half_height * (2.0 / 3.0)
            } else {
                arrow_half_height
            };
            let back_len = arrow_length + arrow_back;
            (tip_gap, -half_height, tip_gap + back_len, half_height)
        }
        MarkerKind::Async => {
            let back_len = arrow_length + arrow_back;
            (
                tip_gap,
                -arrow_half_height,
                tip_gap + back_len,
                arrow_half_height,
            )
        }
        MarkerKind::Box => (0.0, -4.0, 8.0, 4.0),
        MarkerKind::HalfCircle => (-0.0, -8.0, 8.0, 8.0),
        MarkerKind::Dash => (4.0, -4.0, 12.0, 4.0),
        MarkerKind::Cross => (4.5, -4.5, 13.5, 4.5),
        MarkerKind::CirclePlus | MarkerKind::Circle => {
            let radius = 4.0 * stroke_width;
            let center_offset = 5.0 * stroke_width;
            (
                center_offset - radius,
                -radius,
                center_offset + radius,
                radius,
            )
        }
        MarkerKind::BaseDash => (0.0, -4.5, 0.0, 4.5),
        MarkerKind::ERone => (4.5, -4.5, 4.5, 4.5),
        MarkerKind::ERmandOne => (4.5, -4.5, 9.0, 4.5),
        MarkerKind::ERmany => (0.0, -5.0, 10.0, 5.0),
        MarkerKind::ERoneToMany => (0.0, -5.0, 10.0, 5.0),
        MarkerKind::ERzeroToOne => {
            let radius = stroke_width;
            let center_offset = 5.0 * stroke_width;
            let max_x = (center_offset + radius).max(11.5);
            (0.0, -5.0, max_x, 5.0)
        }
        MarkerKind::ERzeroToMany => {
            let radius = stroke_width;
            let center_offset = 5.0 * stroke_width;
            let max_x = (center_offset + radius).max(10.0);
            (0.0, -5.0, max_x, 5.0)
        }
        MarkerKind::DoubleBlock => {
            let back_len = arrow_length + arrow_back;
            (
                tip_gap,
                -arrow_half_height,
                tip_gap + 2.0 * back_len,
                arrow_half_height,
            )
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MarkerKind {
    Classic,
    ClassicThin,
    Open,
    OpenThin,
    OpenAsync,
    Oval,
    Diamond,
    DiamondThin,
    Block,
    BlockThin,
    Async,
    Box,
    HalfCircle,
    Dash,
    Cross,
    CirclePlus,
    Circle,
    BaseDash,
    ERone,
    ERmandOne,
    ERmany,
    ERoneToMany,
    ERzeroToOne,
    ERzeroToMany,
    DoubleBlock,
}

fn marker_kind_from_value(value: Option<&str>) -> Option<MarkerKind> {
    match value? {
        "classic" => Some(MarkerKind::Classic),
        "classicThin" => Some(MarkerKind::ClassicThin),
        "open" => Some(MarkerKind::Open),
        "openThin" => Some(MarkerKind::OpenThin),
        "openAsync" => Some(MarkerKind::OpenAsync),
        "oval" => Some(MarkerKind::Oval),
        "diamond" => Some(MarkerKind::Diamond),
        "diamondThin" => Some(MarkerKind::DiamondThin),
        "block" => Some(MarkerKind::Block),
        "blockThin" => Some(MarkerKind::BlockThin),
        "async" => Some(MarkerKind::Async),
        "box" => Some(MarkerKind::Box),
        "halfCircle" => Some(MarkerKind::HalfCircle),
        "dash" => Some(MarkerKind::Dash),
        "cross" => Some(MarkerKind::Cross),
        "circlePlus" => Some(MarkerKind::CirclePlus),
        "circle" => Some(MarkerKind::Circle),
        "baseDash" => Some(MarkerKind::BaseDash),
        "ERone" => Some(MarkerKind::ERone),
        "ERmandOne" => Some(MarkerKind::ERmandOne),
        "ERmany" => Some(MarkerKind::ERmany),
        "ERoneToMany" => Some(MarkerKind::ERoneToMany),
        "ERzeroToOne" => Some(MarkerKind::ERzeroToOne),
        "ERzeroToMany" => Some(MarkerKind::ERzeroToMany),
        "doubleBlock" => Some(MarkerKind::DoubleBlock),
        _ => None,
    }
}

fn marker_offsets(style: Option<&str>, stroke_width: f64) -> (f64, f64) {
    let shape = style_value(style, "shape");
    if matches!(shape, Some("arrow" | "link" | "flexArrow")) {
        return (0.0, 0.0);
    }
    let (tip_gap, arrow_length, arrow_back, arrow_half_height) = edge_arrow_metrics(stroke_width);
    let start_kind = marker_kind_from_value(style_value(style, "startArrow"));
    let end_kind = match style_value(style, "endArrow") {
        Some("none") => None,
        Some(value) => marker_kind_from_value(Some(value)),
        None => Some(MarkerKind::Classic),
    };
    let start_offset = marker_line_offset(
        start_kind,
        stroke_width,
        tip_gap,
        arrow_length,
        arrow_back,
        arrow_half_height,
    );
    let end_offset = marker_line_offset(
        end_kind,
        stroke_width,
        tip_gap,
        arrow_length,
        arrow_back,
        arrow_half_height,
    );
    (start_offset, end_offset)
}

fn marker_line_offset(
    kind: Option<MarkerKind>,
    stroke_width: f64,
    tip_gap: f64,
    arrow_length: f64,
    arrow_back: f64,
    _arrow_half_height: f64,
) -> f64 {
    match kind {
        None => 0.0,
        Some(MarkerKind::Classic) | Some(MarkerKind::ClassicThin) => tip_gap + arrow_length,
        Some(MarkerKind::Open) | Some(MarkerKind::OpenThin) => tip_gap * 2.0,
        Some(MarkerKind::OpenAsync)
        | Some(MarkerKind::Dash)
        | Some(MarkerKind::Cross)
        | Some(MarkerKind::BaseDash)
        | Some(MarkerKind::ERone)
        | Some(MarkerKind::ERmandOne)
        | Some(MarkerKind::ERmany)
        | Some(MarkerKind::ERoneToMany) => 0.0,
        Some(MarkerKind::ERzeroToOne) | Some(MarkerKind::ERzeroToMany) => stroke_width * 6.0 + 0.5,
        Some(MarkerKind::Oval) => 3.0 * stroke_width,
        Some(MarkerKind::Diamond) => {
            if (stroke_width - 1.0).abs() < f64::EPSILON {
                7.71
            } else {
                tip_gap + arrow_length
            }
        }
        Some(MarkerKind::DiamondThin) => {
            if (stroke_width - 1.0).abs() < f64::EPSILON {
                7.99
            } else {
                tip_gap + arrow_length
            }
        }
        Some(MarkerKind::Block) | Some(MarkerKind::BlockThin) | Some(MarkerKind::Async) => {
            tip_gap + arrow_length + arrow_back
        }
        Some(MarkerKind::Box) | Some(MarkerKind::HalfCircle) => 8.0,
        Some(MarkerKind::Circle) | Some(MarkerKind::CirclePlus) => stroke_width * 9.0,
        Some(MarkerKind::DoubleBlock) => tip_gap + 2.0 * (arrow_length + arrow_back),
    }
}

fn render_edge_markers(
    edge: &MxCell,
    edge_path: &EdgePath,
    min_x: f64,
    min_y: f64,
    stroke_attr: &str,
    stroke_style_attr: &str,
    stroke_width_attr: &str,
) -> String {
    let style = edge.style.as_deref();
    let shape = style_value(style, "shape");
    if matches!(shape, Some("arrow" | "link" | "flexArrow")) {
        return String::new();
    }
    let stroke_width = stroke_width_value(style);
    let (tip_gap, arrow_length, arrow_back, arrow_half_height) = edge_arrow_metrics(stroke_width);
    let start_kind = marker_kind_from_value(style_value(style, "startArrow"));
    let end_kind = match style_value(style, "endArrow") {
        Some("none") => None,
        Some(value) => marker_kind_from_value(Some(value)),
        None => Some(MarkerKind::Classic),
    };
    let start_fill = style_value(style, "startFill") != Some("0");
    let end_fill = style_value(style, "endFill") != Some("0");
    let ctx = MarkerContext {
        stroke_width,
        tip_gap,
        arrow_length,
        arrow_back,
        arrow_half_height,
        min_x,
        min_y,
        stroke_attr,
        stroke_style_attr,
        stroke_width_attr,
    };
    let mut out = String::new();
    if let Some(kind) = start_kind {
        out.push_str(&render_marker(
            kind,
            edge_path.source_anchor,
            edge_path.start_dir,
            start_fill,
            &ctx,
        ));
    }
    if let Some(kind) = end_kind {
        out.push_str(&render_marker(
            kind,
            edge_path.target_anchor,
            Point {
                x: -edge_path.end_dir.x,
                y: -edge_path.end_dir.y,
            },
            end_fill,
            &ctx,
        ));
    }
    out
}

struct MarkerContext<'a> {
    stroke_width: f64,
    tip_gap: f64,
    arrow_length: f64,
    arrow_back: f64,
    arrow_half_height: f64,
    min_x: f64,
    min_y: f64,
    stroke_attr: &'a str,
    stroke_style_attr: &'a str,
    stroke_width_attr: &'a str,
}

fn render_marker(
    kind: MarkerKind,
    anchor: Point,
    dir: Point,
    filled: bool,
    ctx: &MarkerContext<'_>,
) -> String {
    let dir = Point {
        x: (dir.x * 1000.0).round() / 1000.0,
        y: (dir.y * 1000.0).round() / 1000.0,
    };
    let perp = Point {
        x: -dir.y,
        y: dir.x,
    };
    let to_world = |p: Point| Point {
        x: anchor.x + dir.x * p.x + perp.x * p.y,
        y: anchor.y + dir.y * p.x + perp.y * p.y,
    };
    let arrow_half_height = ctx.arrow_half_height;
    let tip_gap = ctx.tip_gap;
    let arrow_length = ctx.arrow_length;
    let arrow_back = ctx.arrow_back;
    let stroke_width = ctx.stroke_width;
    let min_x = ctx.min_x;
    let min_y = ctx.min_y;
    let stroke_attr = ctx.stroke_attr;
    let stroke_width_attr = ctx.stroke_width_attr;
    let fill_value = if filled { ctx.stroke_attr } else { "none" };
    let marker_style_attr = marker_style_attr(ctx.stroke_style_attr, filled);
    let mut out = String::new();
    match kind {
        MarkerKind::Classic | MarkerKind::ClassicThin => {
            let half_height = if kind == MarkerKind::ClassicThin {
                arrow_half_height * (2.0 / 3.0)
            } else {
                arrow_half_height
            };
            let tip = to_world(Point { x: tip_gap, y: 0.0 });
            let base = to_world(Point {
                x: tip_gap + arrow_length,
                y: 0.0,
            });
            let left = to_world(Point {
                x: tip_gap + arrow_length + arrow_back,
                y: -half_height,
            });
            let right = to_world(Point {
                x: tip_gap + arrow_length + arrow_back,
                y: half_height,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {} L {} {} Z\" fill=\"{}\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(tip.x - min_x),
                fmt_num(tip.y - min_y),
                fmt_num(left.x - min_x),
                fmt_num(left.y - min_y),
                fmt_num(base.x - min_x),
                fmt_num(base.y - min_y),
                fmt_num(right.x - min_x),
                fmt_num(right.y - min_y),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Open | MarkerKind::OpenThin => {
            let half_height = if kind == MarkerKind::OpenThin {
                arrow_half_height * (2.0 / 3.0)
            } else {
                arrow_half_height
            };
            let tip = to_world(Point { x: tip_gap, y: 0.0 });
            let left = to_world(Point {
                x: tip_gap + arrow_length + arrow_back,
                y: -half_height,
            });
            let right = to_world(Point {
                x: tip_gap + arrow_length + arrow_back,
                y: half_height,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(left.x - min_x),
                fmt_num(left.y - min_y),
                fmt_num(tip.x - min_x),
                fmt_num(tip.y - min_y),
                fmt_num(right.x - min_x),
                fmt_num(right.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::OpenAsync => {
            let local_tip_gap = 0.0;
            let tip = to_world(Point {
                x: local_tip_gap,
                y: 0.0,
            });
            let back_up = to_world(Point {
                x: local_tip_gap + arrow_length + arrow_back,
                y: -arrow_half_height,
            });
            let back_down = to_world(Point {
                x: local_tip_gap + arrow_length + arrow_back,
                y: arrow_half_height,
            });
            let back = if back_up.y <= back_down.y {
                back_up
            } else {
                back_down
            };
            write!(
                out,
                "<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(tip.x - min_x),
                fmt_num(tip.y - min_y),
                fmt_num(back.x - min_x),
                fmt_num(back.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Oval => {
            let radius = 3.0 * stroke_width;
            let center = to_world(Point { x: 0.0, y: 0.0 });
            write!(
                out,
                "<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{} />",
                fmt_num(center.x - min_x),
                fmt_num(center.y - min_y),
                fmt_num(radius),
                fmt_num(radius),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Diamond | MarkerKind::DiamondThin => {
            let half_width = 3.5;
            let half_height = if kind == MarkerKind::DiamondThin {
                2.06
            } else {
                3.5
            };
            let local_tip_gap = if kind == MarkerKind::DiamondThin {
                0.99
            } else {
                0.71
            };
            let tip = to_world(Point {
                x: local_tip_gap,
                y: 0.0,
            });
            let upper = to_world(Point {
                x: local_tip_gap + half_width,
                y: -half_height,
            });
            let base = to_world(Point {
                x: local_tip_gap + 2.0 * half_width,
                y: 0.0,
            });
            let lower = to_world(Point {
                x: local_tip_gap + half_width,
                y: half_height,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {} L {} {} Z\" fill=\"{}\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(tip.x - min_x),
                fmt_num(tip.y - min_y),
                fmt_num(upper.x - min_x),
                fmt_num(upper.y - min_y),
                fmt_num(base.x - min_x),
                fmt_num(base.y - min_y),
                fmt_num(lower.x - min_x),
                fmt_num(lower.y - min_y),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Block | MarkerKind::BlockThin => {
            let half_height = if kind == MarkerKind::BlockThin {
                arrow_half_height * (2.0 / 3.0)
            } else {
                arrow_half_height
            };
            let back_len = arrow_length + arrow_back;
            let tip = to_world(Point { x: tip_gap, y: 0.0 });
            let upper = to_world(Point {
                x: tip_gap + back_len,
                y: -half_height,
            });
            let lower = to_world(Point {
                x: tip_gap + back_len,
                y: half_height,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {} Z\" fill=\"{}\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(tip.x - min_x),
                fmt_num(tip.y - min_y),
                fmt_num(upper.x - min_x),
                fmt_num(upper.y - min_y),
                fmt_num(lower.x - min_x),
                fmt_num(lower.y - min_y),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Async => {
            let back_len = arrow_length + arrow_back;
            let local_tip_gap = tip_gap;
            let tip = to_world(Point {
                x: local_tip_gap,
                y: 0.0,
            });
            let upper_up = to_world(Point {
                x: local_tip_gap + back_len,
                y: -arrow_half_height,
            });
            let upper_down = to_world(Point {
                x: local_tip_gap + back_len,
                y: arrow_half_height,
            });
            let upper = if upper_up.y <= upper_down.y {
                upper_up
            } else {
                upper_down
            };
            let base = to_world(Point {
                x: local_tip_gap + back_len,
                y: 0.0,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {} Z\" fill=\"{}\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(tip.x - min_x),
                fmt_num(tip.y - min_y),
                fmt_num(upper.x - min_x),
                fmt_num(upper.y - min_y),
                fmt_num(base.x - min_x),
                fmt_num(base.y - min_y),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Box => {
            let size = 8.0;
            let top_left = to_world(Point { x: 0.0, y: -4.0 });
            let top_right = to_world(Point { x: size, y: -4.0 });
            let bottom_right = to_world(Point { x: size, y: 4.0 });
            let bottom_left = to_world(Point { x: 0.0, y: 4.0 });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {} L {} {} Z\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(top_left.x - min_x),
                fmt_num(top_left.y - min_y),
                fmt_num(bottom_left.x - min_x),
                fmt_num(bottom_left.y - min_y),
                fmt_num(bottom_right.x - min_x),
                fmt_num(bottom_right.y - min_y),
                fmt_num(top_right.x - min_x),
                fmt_num(top_right.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::HalfCircle => {
            let radius = 8.0;
            let start = to_world(Point { x: 0.0, y: -radius });
            let mid = to_world(Point { x: radius, y: 0.0 });
            let end = to_world(Point { x: 0.0, y: radius });
            let ctrl_top = to_world(Point {
                x: radius,
                y: -radius,
            });
            let ctrl_bottom = to_world(Point {
                x: radius,
                y: radius,
            });
            write!(
                out,
                "<path d=\"M {} {} Q {} {} {} {} Q {} {} {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(start.x - min_x),
                fmt_num(start.y - min_y),
                fmt_num(ctrl_top.x - min_x),
                fmt_num(ctrl_top.y - min_y),
                fmt_num(mid.x - min_x),
                fmt_num(mid.y - min_y),
                fmt_num(ctrl_bottom.x - min_x),
                fmt_num(ctrl_bottom.y - min_y),
                fmt_num(end.x - min_x),
                fmt_num(end.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Dash => {
            let start = to_world(Point { x: 4.0, y: -4.0 });
            let end = to_world(Point { x: 12.0, y: 4.0 });
            write!(
                out,
                "<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(start.x - min_x),
                fmt_num(start.y - min_y),
                fmt_num(end.x - min_x),
                fmt_num(end.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::Cross => {
            let size = 9.0;
            let center = 9.0;
            let a = to_world(Point {
                x: center - size / 2.0,
                y: -size / 2.0,
            });
            let b = to_world(Point {
                x: center + size / 2.0,
                y: size / 2.0,
            });
            let c = to_world(Point {
                x: center - size / 2.0,
                y: size / 2.0,
            });
            let d = to_world(Point {
                x: center + size / 2.0,
                y: -size / 2.0,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {} M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(a.x - min_x),
                fmt_num(a.y - min_y),
                fmt_num(b.x - min_x),
                fmt_num(b.y - min_y),
                fmt_num(c.x - min_x),
                fmt_num(c.y - min_y),
                fmt_num(d.x - min_x),
                fmt_num(d.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::CirclePlus | MarkerKind::Circle => {
            let radius = 4.0 * stroke_width;
            let center_offset = 5.0 * stroke_width;
            let center = to_world(Point {
                x: center_offset,
                y: 0.0,
            });
            write!(
                out,
                "<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"{}\" stroke=\"{}\" pointer-events=\"all\"{}{} />",
                fmt_num(center.x - min_x),
                fmt_num(center.y - min_y),
                fmt_num(radius),
                fmt_num(radius),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
            if kind == MarkerKind::CirclePlus {
                let left = to_world(Point {
                    x: center_offset - radius,
                    y: 0.0,
                });
                let right = to_world(Point {
                    x: center_offset + radius,
                    y: 0.0,
                });
                let top = to_world(Point {
                    x: center_offset,
                    y: -radius,
                });
                let bottom = to_world(Point {
                    x: center_offset,
                    y: radius,
                });
                write!(
                    out,
                    "<path d=\"M {} {} L {} {} M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                    fmt_num(left.x - min_x),
                    fmt_num(left.y - min_y),
                    fmt_num(right.x - min_x),
                    fmt_num(right.y - min_y),
                    fmt_num(top.x - min_x),
                    fmt_num(top.y - min_y),
                    fmt_num(bottom.x - min_x),
                    fmt_num(bottom.y - min_y),
                    stroke_attr,
                    stroke_width_attr,
                    marker_style_attr
                )
                .unwrap();
            }
        }
        MarkerKind::BaseDash => {
            let length = 9.0;
            let top = to_world(Point {
                x: 0.0,
                y: -length / 2.0,
            });
            let bottom = to_world(Point {
                x: 0.0,
                y: length / 2.0,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(top.x - min_x),
                fmt_num(top.y - min_y),
                fmt_num(bottom.x - min_x),
                fmt_num(bottom.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::ERone => {
            let length = 9.0;
            let top = to_world(Point {
                x: 4.5,
                y: -length / 2.0,
            });
            let bottom = to_world(Point {
                x: 4.5,
                y: length / 2.0,
            });
            write!(
                out,
                "<path d=\"M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(top.x - min_x),
                fmt_num(top.y - min_y),
                fmt_num(bottom.x - min_x),
                fmt_num(bottom.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::ERmandOne => {
            let length = 9.0;
            let offsets = [4.5, 9.0];
            let mut d = String::new();
            for offset in offsets {
                let top = to_world(Point {
                    x: offset,
                    y: -length / 2.0,
                });
                let bottom = to_world(Point {
                    x: offset,
                    y: length / 2.0,
                });
                write!(
                    d,
                    " M {} {} L {} {}",
                    fmt_num(top.x - min_x),
                    fmt_num(top.y - min_y),
                    fmt_num(bottom.x - min_x),
                    fmt_num(bottom.y - min_y)
                )
                .unwrap();
            }
            write!(
                out,
                "<path d=\"{}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                d.trim_start(),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::ERmany => {
            let a = to_world(Point { x: 0.0, y: 5.0 });
            let b = to_world(Point { x: 10.0, y: 0.0 });
            let c = to_world(Point { x: 0.0, y: -5.0 });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(a.x - min_x),
                fmt_num(a.y - min_y),
                fmt_num(b.x - min_x),
                fmt_num(b.y - min_y),
                fmt_num(c.x - min_x),
                fmt_num(c.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::ERoneToMany => {
            let line_len = 10.0;
            let top = to_world(Point {
                x: 10.0,
                y: -line_len / 2.0,
            });
            let bottom = to_world(Point {
                x: 10.0,
                y: line_len / 2.0,
            });
            let a = to_world(Point { x: 0.0, y: 5.0 });
            let b = to_world(Point { x: 10.0, y: 0.0 });
            let c = to_world(Point { x: 0.0, y: -5.0 });
            let mut d = String::new();
            write!(
                d,
                "M {} {} L {} {} M {} {} L {} {} L {} {}",
                fmt_num(top.x - min_x),
                fmt_num(top.y - min_y),
                fmt_num(bottom.x - min_x),
                fmt_num(bottom.y - min_y),
                fmt_num(a.x - min_x),
                fmt_num(a.y - min_y),
                fmt_num(b.x - min_x),
                fmt_num(b.y - min_y),
                fmt_num(c.x - min_x),
                fmt_num(c.y - min_y)
            )
            .unwrap();
            write!(
                out,
                "<path d=\"{}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                d,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::ERzeroToOne => {
            let radius = stroke_width;
            let center_offset = 5.0 * stroke_width;
            let center = to_world(Point {
                x: center_offset,
                y: 0.0,
            });
            write!(
                out,
                "<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"none\" stroke=\"{}\" pointer-events=\"all\"{}{} />",
                fmt_num(center.x - min_x),
                fmt_num(center.y - min_y),
                fmt_num(radius),
                fmt_num(radius),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
            let v_top = to_world(Point { x: 5.0, y: -5.0 });
            let v_bottom = to_world(Point { x: 5.0, y: 5.0 });
            let h_left = to_world(Point { x: 0.0, y: 0.0 });
            let h_right = to_world(Point { x: 11.5, y: 0.0 });
            let (h_start, h_end) = (h_right, h_left);
            write!(
                out,
                "<path d=\"M {} {} L {} {} M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(v_top.x - min_x),
                fmt_num(v_top.y - min_y),
                fmt_num(v_bottom.x - min_x),
                fmt_num(v_bottom.y - min_y),
                fmt_num(h_start.x - min_x),
                fmt_num(h_start.y - min_y),
                fmt_num(h_end.x - min_x),
                fmt_num(h_end.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::ERzeroToMany => {
            let radius = stroke_width;
            let center_offset = 5.0 * stroke_width;
            let center = to_world(Point {
                x: center_offset,
                y: 0.0,
            });
            write!(
                out,
                "<ellipse cx=\"{}\" cy=\"{}\" rx=\"{}\" ry=\"{}\" fill=\"none\" stroke=\"{}\" pointer-events=\"all\"{}{} />",
                fmt_num(center.x - min_x),
                fmt_num(center.y - min_y),
                fmt_num(radius),
                fmt_num(radius),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
            let a = to_world(Point { x: 0.0, y: 5.0 });
            let b = to_world(Point { x: 10.0, y: 0.0 });
            let c = to_world(Point { x: 0.0, y: -5.0 });
            let mid = to_world(Point { x: 0.0, y: 0.0 });
            write!(
                out,
                "<path d=\"M {} {} L {} {} L {} {} M {} {} L {} {}\" fill=\"none\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                fmt_num(a.x - min_x),
                fmt_num(a.y - min_y),
                fmt_num(b.x - min_x),
                fmt_num(b.y - min_y),
                fmt_num(c.x - min_x),
                fmt_num(c.y - min_y),
                fmt_num(b.x - min_x),
                fmt_num(b.y - min_y),
                fmt_num(mid.x - min_x),
                fmt_num(mid.y - min_y),
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
        MarkerKind::DoubleBlock => {
            let back_len = arrow_length + arrow_back;
            let mut d = String::new();
            for idx in 0..2 {
                let tip_x = tip_gap + back_len * idx as f64;
                let tip = to_world(Point { x: tip_x, y: 0.0 });
                let upper = to_world(Point {
                    x: tip_x + back_len,
                    y: -arrow_half_height,
                });
                let lower = to_world(Point {
                    x: tip_x + back_len,
                    y: arrow_half_height,
                });
                write!(
                    d,
                    " M {} {} L {} {} L {} {} Z",
                    fmt_num(tip.x - min_x),
                    fmt_num(tip.y - min_y),
                    fmt_num(upper.x - min_x),
                    fmt_num(upper.y - min_y),
                    fmt_num(lower.x - min_x),
                    fmt_num(lower.y - min_y)
                )
                .unwrap();
            }
            write!(
                out,
                "<path d=\"{}\" fill=\"{}\" stroke=\"{}\" stroke-miterlimit=\"10\" pointer-events=\"all\"{}{} />",
                d.trim_start(),
                fill_value,
                stroke_attr,
                stroke_width_attr,
                marker_style_attr
            )
            .unwrap();
        }
    }
    out
}

fn marker_style_attr(stroke_style_attr: &str, filled: bool) -> String {
    let Some(inner) = stroke_style_attr
        .trim()
        .strip_prefix("style=\"")
        .and_then(|value| value.strip_suffix('"'))
    else {
        return String::new();
    };
    if !filled {
        return format!(" style=\"{inner}\"");
    }
    if let Some(value) = inner
        .strip_prefix("stroke: ")
        .and_then(|value| value.strip_suffix(';'))
    {
        return format!(" style=\"fill: {value}; stroke: {value};\"");
    }
    format!(" style=\"{inner}\"")
}

fn edge_transform(style: Option<&str>) -> Option<&'static str> {
    if (stroke_width_value(style) - 2.0).abs() < f64::EPSILON {
        None
    } else {
        Some("translate(0.5,0.5)")
    }
}

fn style_has_flag(style: Option<&str>, flag: &str) -> bool {
    style
        .map(|value| value.split(';').any(|part| part == flag))
        .unwrap_or(false)
}

fn is_group_cell(cell: &MxCell) -> bool {
    cell.vertex == Some(true) && style_has_flag(cell.style.as_deref(), "group")
}

fn is_cell_visible(cell: &MxCell) -> bool {
    !matches!(
        cell.extra.get("visible").map(String::as_str),
        Some("0") | Some("false")
    )
}

fn is_collapsed(cell: &MxCell) -> bool {
    matches!(
        cell.extra.get("collapsed").map(String::as_str),
        Some("1") | Some("true")
    )
}

fn resolve_visible_terminal<'a>(
    cell: &'a MxCell,
    cell_by_id: &BTreeMap<String, &'a MxCell>,
) -> Option<&'a MxCell> {
    let mut current = cell;
    loop {
        if is_cell_visible(current) {
            return Some(current);
        }
        let parent_id = current.parent.as_ref()?;
        current = *cell_by_id.get(parent_id)?;
    }
}

fn mark_visibility(
    parent_key: &str,
    parent_visible: bool,
    children_by_parent: &BTreeMap<String, Vec<&MxCell>>,
    visible_by_id: &mut BTreeMap<String, bool>,
) {
    let Some(children) = children_by_parent.get(parent_key) else {
        return;
    };
    for child in children {
        let own_visible = is_cell_visible(child);
        let effective = parent_visible && own_visible;
        visible_by_id.insert(child.id.clone(), effective);
        mark_visibility(
            child.id.as_str(),
            effective && !is_collapsed(child),
            children_by_parent,
            visible_by_id,
        );
    }
}

fn rotation_degrees(style: Option<&str>) -> f64 {
    style_value(style, "rotation")
        .and_then(|value| value.parse::<f64>().ok())
        .unwrap_or(0.0)
}

fn label_rotation_degrees(style: Option<&str>) -> f64 {
    let rotation = rotation_degrees(style);
    if rotation.abs() > f64::EPSILON {
        return rotation;
    }
    if style_value(style, "horizontal") == Some("0") {
        -90.0
    } else {
        0.0
    }
}

fn shape_rotation_attr(style: Option<&str>, x: f64, y: f64, width: f64, height: f64) -> String {
    let rotation = rotation_degrees(style);
    if rotation.abs() <= f64::EPSILON {
        return String::new();
    }
    let center_x = x + width / 2.0;
    let center_y = y + height / 2.0;
    format!(
        " transform=\"rotate({},{},{})\"",
        fmt_num_raw(rotation),
        fmt_num_raw(center_x),
        fmt_num_raw(center_y)
    )
}

fn image_rotation_attr(style: Option<&str>, x: f64, y: f64, width: f64, height: f64) -> String {
    let rotation = rotation_degrees(style);
    if rotation.abs() <= f64::EPSILON {
        return String::new();
    }
    let center_x = x + width / 2.0;
    let center_y = y + height / 2.0;
    format!(
        " transform=\"rotate({},{},{})\"",
        fmt_num(rotation),
        fmt_num(center_x),
        fmt_num(center_y)
    )
}

fn rotated_bbox(x: f64, y: f64, width: f64, height: f64, rotation: f64) -> BBox {
    let center_x = x + width / 2.0;
    let center_y = y + height / 2.0;
    let angle = rotation.to_radians();
    let half_w = width / 2.0;
    let half_h = height / 2.0;
    let cos = angle.cos().abs();
    let sin = angle.sin().abs();
    let extent_x = half_w * cos + half_h * sin;
    let extent_y = half_w * sin + half_h * cos;
    BBox {
        min_x: center_x - extent_x,
        min_y: center_y - extent_y,
        max_x: center_x + extent_x,
        max_y: center_y + extent_y,
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
                "nbsp" => out.push('\u{00A0}'),
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

fn normalize_image_href(value: &str) -> String {
    if value.starts_with("data:") {
        if value.contains(";base64,") {
            return value.to_string();
        }
        if let Some((prefix, data)) = value.split_once(',') {
            return format!("{prefix};base64,{data}");
        }
    }
    value.to_string()
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
