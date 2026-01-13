use quick_xml::{Reader, Writer, events::Event};

pub fn pretty_print_svg(content: &str) -> String {
    let mut reader = Reader::from_str(content);
    reader.config_mut().trim_text(false);
    let mut writer = Writer::new_with_indent(Vec::new(), b' ', 2);

    let mut buf = Vec::new();
    loop {
        match reader
            .read_event_into(&mut buf)
            .unwrap_or_else(|err| panic!("Failed to read SVG: {err}"))
        {
            Event::Eof => break,
            e => writer
                .write_event(e)
                .unwrap_or_else(|err| panic!("Failed to write SVG: {err}")),
        }
        buf.clear();
    }
    String::from_utf8(writer.into_inner()).expect("cleaned SVG must be UTF-8")
}
