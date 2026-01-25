use std::{
    env, fs,
    path::{Component, Path, PathBuf},
};

mod helpers;

use xml_c14n::{CanonicalizationMode, CanonicalizationOptions, canonicalize_xml};

use walkdir::WalkDir;

fn parse_drawio_to_svg(drawio_xml: &str) -> String {
    let mx = drawio_core::parse_mxfile(drawio_xml).expect("parse mxfile");
    drawio_core::generate_svg(&mx, 0).expect("generate svg")
}

fn cannonicalized_root() -> PathBuf {
    workspace_root().join("target").join("canonicalized-svg")
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

fn raw_dir() -> PathBuf {
    fixtures_root().join("raw")
}

fn relative_path_for_drawio(drawio_path: &Path) -> PathBuf {
    let rel = drawio_path
        .strip_prefix(corpus_dir())
        .expect("drawio file must be under fixtures/corpus");

    rel.with_extension("svg")
}

fn workspace_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .unwrap_or(&manifest_dir)
        .to_path_buf()
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

fn canonicalize_11_no_comments(xml: &str) -> Result<String, Box<dyn std::error::Error>> {
    let options = CanonicalizationOptions {
        mode: CanonicalizationMode::Canonical1_1,
        keep_comments: false,
        inclusive_ns_prefixes: vec![], // only relevant for exclusive c14n modes
    };
    let canon = canonicalize_xml(xml, options).map_err(|e| format!("c14n error: {e:?}"))?;
    Ok(canon)
}

fn write_svg(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .unwrap_or_else(|e| panic!("Failed to create {}: {e}", parent.display()));
    }
    fs::write(path, content)
        .unwrap_or_else(|e| panic!("Failed to write SVG {}: {e}", path.display()));
}

fn write_canonicalized_svg(path: &Path, role: &str, content: &str) {
    let mut result = PathBuf::new();
    result = result.join(cannonicalized_root());
    if let Some(parent) = path.parent() {
        result = result.join(parent);
    }
    if let Some(steam) = path.file_stem() {
        result = result.join(steam)
    }

    let out_path = result.join(role).with_added_extension("svg");
    let pretty_contenet = helpers::pretty_print_svg(content);
    write_svg(&out_path, &pretty_contenet);
}

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
        let relative_path = relative_path_for_drawio(path);
        let expected_path = expected_dir().join(&relative_path);
        if !expected_path.exists() {
            if env::var_os("UPDATE_EXPECTED").is_some() {
                let raw_path = raw_dir().join(&relative_path);
                if !raw_path.exists() {
                    failures.push(format!(
                        "Missing raw SVG for {}\nExpected at: {}",
                        path.display(),
                        expected_path.display()
                    ));
                }
                let expected_raw_svg = read_to_string(&raw_path);
                let expected_clean = helpers::clean_svg(expected_raw_svg.as_str());
                let expected_norm = canonicalize_11_no_comments(expected_clean.as_str()).unwrap();
                write_svg(&expected_path, &expected_norm);
            } else {
                failures.push(format!(
                    "Missing expected SVG for {}\nExpected at: {}, use set UPDATE_EXPECTED env when running test to generated it.",
                    path.display(),
                    expected_path.display()
                ));
            }
            continue;
        }

        let drawio_xml = read_to_string(path);
        let actual_svg = parse_drawio_to_svg(&drawio_xml);
        let expected_clean = read_to_string(&expected_path);
        let actual_clean = helpers::clean_svg(actual_svg.as_str());
        let expected_norm = canonicalize_11_no_comments(expected_clean.as_str()).unwrap();
        let actual_norm = canonicalize_11_no_comments(actual_clean.as_str()).unwrap();

        if expected_norm != actual_norm {
            if env::var_os("KEEP_CANNONICALIZED_SVG").is_some() {
                write_canonicalized_svg(&relative_path, "actual", &actual_norm);
                write_canonicalized_svg(&relative_path, "expected", &expected_norm);
                failures.push(format!(
                    "Mismatch for fixture: {}\nExpected: {}\n--- Expected ---\n{}\n--- Actual ---\n{}\n--- Files at ---\n{}",
                    path.display(),
                    expected_path.display(),
                    expected_norm,
                    actual_norm,
                    cannonicalized_root().display(),
                ));
            } else {
                failures.push(format!(
                    "Mismatch for fixture: {}\nExpected: {}\n--- Expected ---\n{}\n--- Actual ---\n{}",
                    path.display(),
                    expected_path.display(),
                    expected_norm,
                    actual_norm
                ));
            }
        };
    }

    if !failures.is_empty() {
        panic!(
            "SVG dump mismatches found ({}):\n\n{}",
            failures.len(),
            failures.join("\n\n---\n\n")
        );
    }
}
