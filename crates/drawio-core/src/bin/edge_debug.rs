#[cfg(feature = "edge-debug")]
mod real {
    use drawio_core::generate_svg;
    use drawio_core::model::MxCell;
    use drawio_core::parse_mxfile;
    use drawio_core::svg::{
        DebugEdgeGeometry, DebugGraphBounds, SvgResult, debug_edge_geometry, debug_graph_bounds,
    };
    use quick_xml::Reader;
    use quick_xml::events::Event;
    use serde::Serialize;
    use std::collections::BTreeMap;
    use std::fs;
    use std::path::PathBuf;

    #[derive(Clone, Serialize)]
    struct PathInfo {
        attrs: BTreeMap<String, String>,
    }

    #[derive(Serialize)]
    struct EdgeDebugReport {
        edge_id: String,
        geometry: Option<DebugEdgeGeometry>,
        found_cell_ids: Vec<String>,
        used_fallback: bool,
        actual_paths: Vec<PathInfo>,
        expected_paths: Vec<PathInfo>,
    }

    fn build_cell_map(cells: &[MxCell]) -> BTreeMap<String, &MxCell> {
        let mut map = BTreeMap::new();
        for cell in cells {
            map.insert(cell.id.clone(), cell);
        }
        map
    }

    fn extract_paths(svg: &str, cell_id: &str) -> (Vec<PathInfo>, Vec<String>, bool) {
        let mut reader = Reader::from_str(svg);
        let mut buf = Vec::new();
        let mut depth: usize = 0;
        let mut cell_depth: Option<usize> = None;
        let mut paths = Vec::new();
        let mut found_cell_ids: Vec<String> = Vec::new();
        let mut used_fallback = false;
        let mut all_paths: Vec<PathInfo> = Vec::new();

        loop {
            match reader.read_event_into(&mut buf) {
                Ok(Event::Start(ref event)) => {
                    depth += 1;
                    if event.name().as_ref().ends_with(b"g") {
                        let mut match_cell = false;
                        for attr in event.attributes().flatten() {
                            if attr.key.as_ref() == b"data-cell-id" {
                                if let Ok(value) = attr.unescape_value() {
                                    found_cell_ids.push(value.to_string());
                                    if value.as_ref() == cell_id {
                                        match_cell = true;
                                    }
                                }
                            }
                        }
                        if match_cell {
                            cell_depth = Some(depth);
                        }
                    }
                    if event.name().as_ref().ends_with(b"path") {
                        let mut attrs = BTreeMap::new();
                        for attr in event.attributes().flatten() {
                            if let Ok(value) = attr.unescape_value() {
                                let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
                                attrs.insert(key, value.to_string());
                            }
                        }
                        if cell_depth.is_some() {
                            paths.push(PathInfo { attrs });
                        } else {
                            all_paths.push(PathInfo { attrs });
                        }
                    }
                }
                Ok(Event::Empty(ref event)) => {
                    if event.name().as_ref().ends_with(b"path") {
                        let mut attrs = BTreeMap::new();
                        for attr in event.attributes().flatten() {
                            if let Ok(value) = attr.unescape_value() {
                                let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
                                attrs.insert(key, value.to_string());
                            }
                        }
                        if cell_depth.is_some() {
                            paths.push(PathInfo { attrs });
                        } else {
                            all_paths.push(PathInfo { attrs });
                        }
                    }
                }
                Ok(Event::End(ref event)) => {
                    if let Some(active_depth) = cell_depth {
                        if depth == active_depth && event.name().as_ref().ends_with(b"g") {
                            cell_depth = None;
                        }
                    }
                    if depth > 0 {
                        depth -= 1;
                    }
                }
                Ok(Event::Eof) => break,
                Err(_) => break,
                _ => {}
            }
            buf.clear();
        }

        if paths.is_empty() && !all_paths.is_empty() {
            used_fallback = true;
            paths.push(all_paths[0].clone());
        }

        found_cell_ids.sort();
        found_cell_ids.dedup();
        (paths, found_cell_ids, used_fallback)
    }

    fn run_bounds(drawio_path: PathBuf, diagram_index: usize) -> SvgResult<()> {
        let drawio_xml = fs::read_to_string(&drawio_path).expect("failed to read drawio input");
        let mxfile = parse_mxfile(&drawio_xml).expect("parse mxfile");
        let bounds: DebugGraphBounds = debug_graph_bounds(&mxfile, diagram_index)?;
        let output = serde_json::to_string_pretty(&bounds).expect("serialize bounds report");
        println!("{output}");
        Ok(())
    }

    fn run_edge_debug(
        drawio_path: PathBuf,
        edge_id: String,
        expected_svg_path: PathBuf,
    ) -> SvgResult<()> {
        let drawio_xml = fs::read_to_string(&drawio_path).expect("failed to read drawio input");
        let mxfile = parse_mxfile(&drawio_xml).expect("parse mxfile");
        let diagram = mxfile
            .diagrams
            .get(0)
            .and_then(|diagram| diagram.graph_model.as_ref())
            .expect("diagram graph model");
        let cell_by_id = build_cell_map(&diagram.root.cells);
        let edge = cell_by_id
            .get(&edge_id)
            .copied()
            .expect("edge cell not found");

        let geometry = debug_edge_geometry(edge, &cell_by_id)?;
        let actual_svg = generate_svg(&mxfile, 0)?;
        let expected_svg =
            fs::read_to_string(&expected_svg_path).expect("failed to read expected svg");

        let (actual_paths, actual_ids, actual_fallback) =
            extract_paths(&actual_svg, edge.id.as_str());
        let (expected_paths, expected_ids, expected_fallback) =
            extract_paths(&expected_svg, edge.id.as_str());

        let mut found_cell_ids = actual_ids;
        found_cell_ids.extend(expected_ids);
        found_cell_ids.sort();
        found_cell_ids.dedup();

        let report = EdgeDebugReport {
            edge_id,
            geometry,
            found_cell_ids,
            used_fallback: actual_fallback || expected_fallback,
            actual_paths,
            expected_paths,
        };

        let output = serde_json::to_string_pretty(&report).expect("serialize report");
        println!("{output}");
        Ok(())
    }

    fn run() -> SvgResult<()> {
        let mut args = std::env::args().skip(1);
        let Some(first) = args.next() else {
            eprintln!(
                "usage: edge_debug bounds <drawio> [diagram_index]\n\
                usage: edge_debug <drawio> <edge_id> <expected_svg>"
            );
            std::process::exit(2);
        };

        if first == "bounds" {
            let drawio_path = PathBuf::from(args.next().expect("drawio path"));
            let diagram_index = args
                .next()
                .map(|value| value.parse::<usize>().expect("diagram_index"))
                .unwrap_or(0);
            return run_bounds(drawio_path, diagram_index);
        }

        let drawio_path = PathBuf::from(first);
        let edge_id = args.next().expect("edge id");
        let expected_svg_path = PathBuf::from(args.next().expect("expected svg path"));

        run_edge_debug(drawio_path, edge_id, expected_svg_path)
    }

    pub fn main() {
        if let Err(err) = run() {
            eprintln!("error: {err}");
            std::process::exit(1);
        }
    }
}

#[cfg(feature = "edge-debug")]
fn main() {
    real::main();
}

#[cfg(not(feature = "edge-debug"))]
fn main() {
    eprintln!("edge_debug requires `--features edge-debug`");
    std::process::exit(2);
}
