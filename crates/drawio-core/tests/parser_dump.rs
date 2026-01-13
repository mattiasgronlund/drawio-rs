use std::{
    env, fs,
    path::Component,
    path::{Path, PathBuf},
};

use serde_json::Value;
use similar::TextDiff;
use std::collections::HashSet;
use walkdir::WalkDir;

/// If you want stable comparisons, normalize JSON:
/// - ensure objects have stable key order (serde_json::Map keeps insertion order, not sorted)
/// - remove nulls
/// - optionally remove empty arrays/objects
fn normalize_json(value: &mut Value) {
    match value {
        Value::Null => {}
        Value::Bool(_) => {}
        Value::Number(_) => {}
        Value::String(_) => {}
        Value::Array(arr) => {
            for v in arr.iter_mut() {
                normalize_json(v);
            }
            // Optional: drop nulls inside arrays (comment out if you don't want this)
            // arr.retain(|v| !v.is_null());
        }
        Value::Object(map) => {
            // First normalize children
            let keys: Vec<String> = map.keys().cloned().collect();
            for k in keys {
                if let Some(v) = map.get_mut(&k) {
                    normalize_json(v);
                }
            }

            // Remove null fields to reduce noise in fixtures
            map.retain(|_, v| !v.is_null());

            // Rebuild with sorted keys for deterministic pretty printing
            let mut entries: Vec<(String, Value)> =
                map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            entries.sort_by(|a, b| a.0.cmp(&b.0));

            let mut new_map = serde_json::Map::new();
            for (k, v) in entries {
                new_map.insert(k, v);
            }

            *map = new_map;
        }
    }
}

fn pretty_json(mut v: Value) -> String {
    normalize_json(&mut v);
    // 2-space indent for readability; stable key ordering done in normalize_json
    let mut s = serde_json::to_string_pretty(&v).expect("serialize pretty json");
    s.push('\n');
    s
}

fn fixtures_root() -> PathBuf {
    // Repo root is not necessarily cwd in all runners; this assumes tests run from crate dir.
    // Using CARGO_MANIFEST_DIR = crates/drawio-core
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent() // crates/
        .and_then(|p| p.parent()) // repo root
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
    // drawio_path is under fixtures/corpus/...
    let rel = drawio_path
        .strip_prefix(corpus_dir())
        .expect("drawio file must be under fixtures/corpus");

    expected_dir().join(rel).with_extension("json") // replaces .drawio/.xml with .json
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

fn ignored_fixtures() -> HashSet<String> {
    let ignore_path = fixtures_ignore_path();
    if !ignore_path.exists() {
        return HashSet::new();
    }

    let contents = read_to_string(&ignore_path);
    contents
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(|line| line.to_string())
        .collect()
}

fn is_update_enabled() -> bool {
    match env::var("UPDATE_EXPECTED") {
        Ok(v) => !v.trim().is_empty() && v != "0" && v.to_lowercase() != "false",
        Err(_) => false,
    }
}

fn read_to_string(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {}: {e}", path.display()))
}

fn write_string(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .unwrap_or_else(|e| panic!("Failed to create dir {}: {e}", parent.display()));
    }
    fs::write(path, contents).unwrap_or_else(|e| panic!("Failed to write {}: {e}", path.display()));
}

fn diff_strings(expected: &str, actual: &str) -> String {
    let diff = TextDiff::from_lines(expected, actual);

    // Unified diff is easiest to read in CI logs
    diff.unified_diff().header("expected", "actual").to_string()
}

#[test]
fn parser_dump_matches_expected_json() {
    let corpus = corpus_dir();
    assert!(
        corpus.exists(),
        "Corpus directory not found at {}",
        corpus.display()
    );

    let update = is_update_enabled();
    let ignore = ignored_fixtures();
    let mut failures: Vec<String> = Vec::new();

    // Walk all files and pick .drawio / .dio / .drawio.xml etc if you want.
    // For now: anything ending with .drawio
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
        if ignore.contains(&rel_path) {
            continue;
        }

        let expected_path = expected_path_for_drawio(path);

        let drawio_xml = read_to_string(path);
        let actual_json_val = drawio_core::parse_mxfile(&drawio_xml)
            .unwrap_or_else(|err| panic!("parse mxfile for {}: {err}", path.display()));
        let actual_json_val = serde_json::to_value(&actual_json_val).expect("mxfile -> json");
        let actual_pretty = pretty_json(actual_json_val);

        if !expected_path.exists() {
            if update {
                write_string(&expected_path, &actual_pretty);
                // Treat as success after writing
                continue;
            } else {
                failures.push(format!(
                    "Missing expected JSON for {}\nExpected at: {}\nRun with UPDATE_EXPECTED=1 to generate.",
                    path.display(),
                    expected_path.display()
                ));
                continue;
            }
        }

        let expected_raw = read_to_string(&expected_path);

        // Normalize expected too by parsing and re-prettying, so old key-ordering doesn't cause noise.
        let expected_val: Value = serde_json::from_str(&expected_raw).unwrap_or_else(|e| {
            panic!(
                "Invalid JSON in expected file {}: {e}",
                expected_path.display()
            )
        });
        let expected_pretty = pretty_json(expected_val);

        if expected_pretty != actual_pretty {
            if update {
                write_string(&expected_path, &actual_pretty);
                // After updating, consider it fixed for this run.
                continue;
            }

            let d = diff_strings(&expected_pretty, &actual_pretty);
            failures.push(format!(
                "Mismatch for fixture: {}\nExpected: {}\n{}\nTip: set UPDATE_EXPECTED=1 to update fixtures.",
                path.display(),
                expected_path.display(),
                d
            ));
        }
    }

    if !failures.is_empty() {
        panic!(
            "Parser dump mismatches found ({}):\n\n{}",
            failures.len(),
            failures.join("\n\n---\n\n")
        );
    }
}
