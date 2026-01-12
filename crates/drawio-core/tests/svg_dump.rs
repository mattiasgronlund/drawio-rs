use std::{
    env, fs,
    path::{Component, Path, PathBuf},
};

use quick_xml::{
    Reader, Writer,
    errors::IllFormedError,
    events::{BytesEnd, Event},
};
use xml_c14n::{CanonicalizationMode, CanonicalizationOptions, canonicalize_xml};

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

fn raw_dir() -> PathBuf {
    fixtures_root().join("raw")
}

fn relative_path_for_drawio(drawio_path: &Path) -> PathBuf {
    let rel = drawio_path
        .strip_prefix(corpus_dir())
        .expect("drawio file must be under fixtures/corpus");

    rel.with_extension("svg")
}

fn path_for_normalized_svg(root: &Path, path: &Path, role: &str) -> PathBuf {
    let mut result = PathBuf::new();
    result = result.join(root);
    if let Some(parent) = path.parent() {
        result = result.join(parent);
    }
    if let Some(steam) = path.file_stem() {
        result = result.join(steam)
    }

    result.join(role).with_added_extension("svg")
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

fn write_normalized_svg(root: &Path, path: &Path, role: &str, content: &str) {
    let generated_path = path_for_normalized_svg(root, path, role);
    write_svg(&generated_path, content);
}

struct SwitchContent {
    end: BytesEnd<'static>,
    events: Vec<Event<'static>>,
    foreign_range: Option<(usize, usize)>,
    has_foreign: bool,
    has_other: bool,
}

fn read_switch(reader: &mut Reader<&[u8]>) -> Result<SwitchContent, quick_xml::Error> {
    let mut buf = Vec::new();
    let mut events = Vec::new();
    let mut depth = 0usize;
    let mut has_foreign = false;
    let mut has_other = false;
    let mut foreign_range: Option<(usize, usize)> = None;
    let mut foreign_start_idx: Option<usize> = None;
    let mut foreign_capture_depth: Option<usize> = None;

    let end = loop {
        match reader.read_event_into(&mut buf)? {
            Event::Start(e) => {
                let local = e.local_name();
                if depth == 0 {
                    match local.as_ref() {
                        b"foreignObject" => {
                            has_foreign = true;
                        }
                        b"image" => {}
                        _ => {
                            has_other = true;
                        }
                    }
                    if local.as_ref() == b"foreignObject"
                        && foreign_range.is_none()
                        && foreign_capture_depth.is_none()
                    {
                        foreign_start_idx = Some(events.len());
                        foreign_capture_depth = Some(0);
                    }
                }
                if let Some(d) = foreign_capture_depth {
                    foreign_capture_depth = Some(d + 1);
                }
                events.push(Event::Start(e.into_owned()));
                depth += 1;
            }
            Event::Empty(e) => {
                let local = e.local_name();
                if depth == 0 {
                    match local.as_ref() {
                        b"foreignObject" => {
                            has_foreign = true;
                            if foreign_range.is_none() {
                                let start = events.len();
                                events.push(Event::Empty(e.into_owned()));
                                foreign_range = Some((start, events.len()));
                                buf.clear();
                                continue;
                            }
                        }
                        b"image" => {}
                        _ => {
                            has_other = true;
                        }
                    }
                }
                events.push(Event::Empty(e.into_owned()));
            }
            Event::End(e) => {
                if e.local_name().as_ref() == b"switch" && depth == 0 {
                    break e.into_owned();
                }
                events.push(Event::End(e.into_owned()));
                if let Some(d) = foreign_capture_depth {
                    let new_d = d - 1;
                    if new_d == 0 {
                        if let Some(start) = foreign_start_idx {
                            foreign_range = Some((start, events.len()));
                        }
                        foreign_capture_depth = None;
                        foreign_start_idx = None;
                    } else {
                        foreign_capture_depth = Some(new_d);
                    }
                }
                depth = depth.saturating_sub(1);
            }
            Event::Eof => {
                return Err(quick_xml::Error::IllFormed(IllFormedError::MissingEndTag(
                    "switch".to_string(),
                )));
            }
            Event::Text(e) => events.push(Event::Text(e.into_owned())),
            Event::Comment(e) => events.push(Event::Comment(e.into_owned())),
            Event::CData(e) => events.push(Event::CData(e.into_owned())),
            Event::Decl(e) => events.push(Event::Decl(e.into_owned())),
            Event::PI(e) => events.push(Event::PI(e.into_owned())),
            Event::DocType(e) => events.push(Event::DocType(e.into_owned())),
            Event::GeneralRef(e) => events.push(Event::GeneralRef(e.into_owned())),
        }
        buf.clear();
    };

    Ok(SwitchContent {
        end,
        events,
        foreign_range,
        has_foreign,
        has_other,
    })
}

fn clean_svg(content: &str) -> String {
    let mut reader = Reader::from_str(content);
    reader.config_mut().trim_text(false);
    let mut writer = Writer::new(Vec::new());
    let mut buf = Vec::new();

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Eof) => break,
            Ok(Event::Start(e)) if e.local_name().as_ref() == b"switch" => {
                let switch_content = read_switch(&mut reader)
                    .unwrap_or_else(|err| panic!("Failed to parse switch: {err}"));
                if let Some((start, end)) = switch_content
                    .foreign_range
                    .filter(|_| switch_content.has_foreign && !switch_content.has_other)
                {
                    for event in switch_content
                        .events
                        .into_iter()
                        .skip(start)
                        .take(end - start)
                    {
                        writer
                            .write_event(event)
                            .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
                    }
                } else {
                    writer
                        .write_event(Event::Start(e.into_owned()))
                        .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
                    for event in switch_content.events {
                        writer
                            .write_event(event)
                            .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
                    }
                    writer
                        .write_event(Event::End(switch_content.end))
                        .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
                }
            }
            Ok(Event::Empty(e)) if e.local_name().as_ref() == b"switch" => {
                writer
                    .write_event(Event::Empty(e.into_owned()))
                    .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
            }
            Ok(event) => {
                writer
                    .write_event(event.into_owned())
                    .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
            }
            Err(err) => panic!("Failed to parse SVG: {err}"),
        }
        buf.clear();
    }

    String::from_utf8(writer.into_inner()).expect("cleaned SVG must be UTF-8")
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
            if env::var_os("UPDATE_FIXTURES").is_some() {
                let raw_path = raw_dir().join(&relative_path);
                if !raw_path.exists() {
                    failures.push(format!(
                        "Missing raw SVG for {}\nExpected at: {}",
                        path.display(),
                        expected_path.display()
                    ));
                }
                let expected_raw_svg = read_to_string(&raw_path);
                let expected_clean = clean_svg(expected_raw_svg.as_str());
                let expected_norm = canonicalize_11_no_comments(expected_clean.as_str()).unwrap();
                write_svg(&expected_path, &expected_norm);
            } else {
                failures.push(format!(
                    "Missing expected SVG for {}\nExpected at: {}, use set UPDATE_FIXTURES env when running test to generated it.",
                    path.display(),
                    expected_path.display()
                ));
            }
            continue;
        }

        let drawio_xml = read_to_string(path);
        let actual_svg = parse_drawio_to_svg(&drawio_xml);
        let expected_raw = read_to_string(&expected_path);
        let expected_clean = clean_svg(expected_raw.as_str());
        let actual_clean = clean_svg(actual_svg.as_str());
        let expected_norm = canonicalize_11_no_comments(expected_clean.as_str()).unwrap();
        let actual_norm = canonicalize_11_no_comments(actual_clean.as_str()).unwrap();

        if expected_norm != actual_norm {
            if env::var_os("KEEP_NORMALIZED_SVG").is_some() {
                let root = workspace_root().join("target").join("normalized-svg");
                write_normalized_svg(&root, &relative_path, "actual", &actual_norm);
                write_normalized_svg(&root, &relative_path, "expected", &expected_norm);
                failures.push(format!(
                "Mismatch for fixture: {}\nExpected: {}\n--- Expected ---\n{}\n--- Actual ---\n{}\n--- Files at ---\n{}",
                path.display(),
                expected_path.display(),
                expected_norm,
                actual_norm,
                root.display(),
            ));
            } else {
                failures.push(format!(
                    "Mismatch for fixture: {}\nExpected: {}\n--- Expected ---\n{}\n--- Actual ---\n{}",
                    path.display(),
                    expected_path.display(),
                    expected_norm,
                    actual_norm
                ));
            };
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
