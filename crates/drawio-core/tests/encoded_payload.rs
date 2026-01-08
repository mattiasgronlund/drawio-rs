use base64::Engine;
use base64::engine::general_purpose::STANDARD;
use flate2::Compression;
use flate2::read::DeflateEncoder;
use percent_encoding::percent_decode_str;
use percent_encoding::utf8_percent_encode;
use percent_encoding::{AsciiSet, NON_ALPHANUMERIC};
use std::io::Read;

const ENCODE_SET: &AsciiSet = &NON_ALPHANUMERIC
    .remove(b'-')
    .remove(b'_')
    .remove(b'.')
    .remove(b'!')
    .remove(b'~')
    .remove(b'*')
    .remove(b'\'')
    .remove(b'(')
    .remove(b')');

fn encode_diagram_payload(xml: &str) -> String {
    let encoded_xml = utf8_percent_encode(xml, ENCODE_SET).to_string();
    let mut encoder = DeflateEncoder::new(encoded_xml.as_bytes(), Compression::default());
    let mut compressed = Vec::new();
    encoder
        .read_to_end(&mut compressed)
        .expect("deflate encode");
    STANDARD.encode(compressed)
}

#[test]
fn parses_encoded_diagram_payload() {
    let graph_xml = r#"<mxGraphModel dx="10" dy="20"><root><mxCell id="0"/><mxCell id="1" parent="0"/><mxCell id="2" value="Hello" vertex="1" parent="1"><mxGeometry x="1" y="2" width="3" height="4" as="geometry"/></mxCell></root></mxGraphModel>"#;
    let payload = encode_diagram_payload(graph_xml);
    let mxfile = format!(r#"<mxfile host="test"><diagram id="d1">{payload}</diagram></mxfile>"#);

    let parsed = drawio_core::parse_mxfile(&mxfile).expect("parse mxfile");
    let diagram = parsed.diagrams.first().expect("diagram");
    let model = diagram.graph_model.as_ref().expect("decoded graph model");

    assert_eq!(model.dx, Some(10));
    assert_eq!(model.dy, Some(20));
    assert_eq!(model.root.cells.len(), 3);
    assert_eq!(model.root.cells[2].value.as_deref(), Some("Hello"));
    assert_eq!(
        model.root.cells[2].geometry.as_ref().unwrap().width,
        Some(3.0)
    );

    let roundtrip_payload = diagram.encoded_payload.as_ref().expect("payload");
    let decoded_bytes = STANDARD.decode(roundtrip_payload).expect("base64");
    let mut decoder = flate2::read::DeflateDecoder::new(&decoded_bytes[..]);
    let mut inflated = Vec::new();
    decoder.read_to_end(&mut inflated).expect("inflate");
    let inflated_str = std::str::from_utf8(&inflated).expect("utf8");
    let decoded = percent_decode_str(inflated_str)
        .decode_utf8()
        .expect("percent decode");
    assert!(decoded.contains("mxGraphModel"));
}
