use std::{
    env, fs,
    path::Component,
    path::{Path, PathBuf},
};

use walkdir::WalkDir;

fn parse_drawio_to_svg(drawio_xml: &str) -> String {
    let mx = drawio_core::parse_mxfile(drawio_xml).expect("parse mxfile");
    drawio_core::generate_svg(&mx, 0).expect("generate svg")
}

fn fixtures_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .unwrap_or(&manifest_dir)
        .join("fixtures")
}

fn fixtures_ignore_path() -> PathBuf {
    fixtures_root().join("ignore.txt")
}

fn corpus_dir() -> PathBuf {
    fixtures_root().join("corpus")
}

fn expected_dir() -> PathBuf {
    fixtures_root().join("expected")
}

fn expected_path_for_drawio(drawio_path: &Path) -> PathBuf {
    let rel = drawio_path
        .strip_prefix(corpus_dir())
        .expect("drawio file must be under fixtures/corpus");

    expected_dir().join(rel).with_extension("svg")
}

fn rel_fixture_path(path: &Path) -> String {
    path.components()
        .filter_map(|c| match c {
            Component::Normal(p) => Some(p.to_string_lossy()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("/")
}

fn ignored_fixtures() -> Vec<String> {
    let ignore_path = fixtures_ignore_path();
    if !ignore_path.exists() {
        return Vec::new();
    }

    let contents = read_to_string(&ignore_path);
    contents
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(|line| line.to_string())
        .collect()
}

fn read_to_string(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {}: {e}", path.display()))
}

fn normalize_svg(svg: &str) -> String {
    let mut normalized = strip_content_attribute(svg);
    normalized.retain(|c| c != '\n' && c != '\r' && c != '\t');
    normalized.trim().to_string()
}

fn strip_content_attribute(svg: &str) -> String {
    let attr = " content=\"";
    if let Some(start) = svg.find(attr) {
        let after_attr = start + attr.len();
        if let Some(end_quote) = svg[after_attr..].find('"') {
            let mut out = String::with_capacity(svg.len());
            out.push_str(&svg[..start]);
            out.push_str(&svg[after_attr + end_quote + 1..]);
            return out;
        }
    }
    svg.to_string()
}

#[ignore]
#[test]
fn svg_dump_matches_expected_svg() {
    let corpus = corpus_dir();
    assert!(
        corpus.exists(),
        "Corpus directory not found at {}",
        corpus.display()
    );

    let ignore = ignored_fixtures();
    let mut failures: Vec<String> = Vec::new();

    for entry in WalkDir::new(&corpus)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
    {
        let path = entry.path();
        let is_drawio = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|ext| ext.eq_ignore_ascii_case("drawio"))
            .unwrap_or(false);

        if !is_drawio {
            continue;
        }

        let rel = path
            .strip_prefix(corpus_dir())
            .expect("drawio file must be under fixtures/corpus");
        let rel_path = rel_fixture_path(rel);
        if ignore.iter().any(|p| p == &rel_path) {
            continue;
        }

        let expected_path = expected_path_for_drawio(path);
        if !expected_path.exists() {
            failures.push(format!(
                "Missing expected SVG for {}\nExpected at: {}",
                path.display(),
                expected_path.display()
            ));
            continue;
        }

        let drawio_xml = read_to_string(path);
        let actual_svg = parse_drawio_to_svg(&drawio_xml);
        let expected_svg = read_to_string(&expected_path);

        let expected_norm = normalize_svg(&expected_svg);
        let actual_norm = normalize_svg(&actual_svg);

        if expected_norm != actual_norm {
            failures.push(format!(
                "Mismatch for fixture: {}\nExpected: {}\n--- Expected ---\n{}\n--- Actual ---\n{}",
                path.display(),
                expected_path.display(),
                expected_norm,
                actual_norm
            ));
        }
    }

    if !failures.is_empty() {
        panic!(
            "SVG dump mismatches found ({}):\n\n{}",
            failures.len(),
            failures.join("\n\n---\n\n")
        );
    }
}
