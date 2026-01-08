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
        let w = width + 1.0;
        let h = height + 1.0;
        (w, h, w, h)
    };

    write!(
        out,
        "<svg xmlns=\"http://www.w3.org/2000/svg\" style=\"background: transparent; background-color: transparent; color-scheme: light dark;\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\" width=\"{}px\" height=\"{}px\" viewBox=\"-0.5 -0.5 {} {}\">",
        fmt_num(svg_width),
        fmt_num(svg_height),
        fmt_num(view_width),
        fmt_num(view_height)
    )
    .unwrap();

    out.push_str("<defs/>");
    if vertices.is_empty() && edges.is_empty() {
        out.push_str("<g/>");
        out.push_str("</svg>");
        return Ok(out);
    }

    out.push_str("<g>");
    if !edges.is_empty() {
        out.push_str("<g>");
        for edge in edges {
            if let Some((line, arrow)) = render_edge(edge, &cell_by_id, min_x, min_y)? {
                out.push_str(&line);
                out.push_str(&arrow);
            }
        }
        out.push_str("</g>");
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
        out.push_str("<g>");
        write!(
            out,
            "<rect x=\"{}\" y=\"{}\" width=\"{}\" height=\"{}\" fill=\"#ffffff\" stroke=\"#000000\" pointer-events=\"all\" style=\"fill: light-dark(#ffffff, var(--ge-dark-color, #121212)); stroke: light-dark(rgb(0, 0, 0), rgb(255, 255, 255));\"/>",
            fmt_num(x),
            fmt_num(y),
            fmt_num(width),
            fmt_num(height)
        )
        .unwrap();
        out.push_str("</g>");
    }

    out.push_str("</g>");
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

fn render_edge(
    edge: &MxCell,
    cell_by_id: &BTreeMap<String, &MxCell>,
    min_x: f64,
    min_y: f64,
) -> SvgResult<Option<(String, String)>> {
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
    Ok(Some((line, arrow)))
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
