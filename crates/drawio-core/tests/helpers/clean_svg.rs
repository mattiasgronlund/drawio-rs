use quick_xml::{
    Reader, Writer,
    errors::IllFormedError,
    events::{BytesEnd, BytesStart, BytesText, Event},
};

struct SwitchContent {
    end: BytesEnd<'static>,
    events: Vec<Event<'static>>,
    foreign_range: Option<(usize, usize)>,
    has_foreign: bool,
    has_image: bool,
    has_other: bool,
    has_warning_link: bool,
    has_required_features: bool,
}

const WARNING_SWITCH: &str = "<switch><g requiredFeatures=\"http://www.w3.org/TR/SVG11/feature#Extensibility\"/><a transform=\"translate(0,-5)\" xlink:href=\"https://www.drawio.com/doc/faq/svg-export-text-problems\" target=\"_blank\"><text text-anchor=\"middle\" font-size=\"10px\" x=\"50%\" y=\"100%\">Unsupported SVG features detected.\nPlease view this diagram in a modern web browser.</text></a></switch>";

fn start_has_required_features(start: &BytesStart<'_>) -> Result<bool, quick_xml::Error> {
    const REQUIRED_FEATURE: &[u8] = b"http://www.w3.org/TR/SVG11/feature#Extensibility";
    for attr in start.attributes().with_checks(false) {
        let attr = attr?;
        if attr.key.as_ref() == b"requiredFeatures" && attr.value.as_ref() == REQUIRED_FEATURE {
            return Ok(true);
        }
    }
    Ok(false)
}

fn start_has_warning_link(start: &BytesStart<'_>) -> Result<bool, quick_xml::Error> {
    const WARNING_LINK: &[u8] = b"https://www.drawio.com/doc/faq/svg-export-text-problems";
    for attr in start.attributes().with_checks(false) {
        let attr = attr?;
        if attr.key.as_ref() == b"xlink:href" && attr.value.as_ref() == WARNING_LINK {
            return Ok(true);
        }
    }
    Ok(false)
}

fn read_switch(reader: &mut Reader<&[u8]>) -> Result<SwitchContent, quick_xml::Error> {
    let mut buf = Vec::new();
    let mut events = Vec::new();
    let mut depth = 0usize;
    let mut has_foreign = false;
    let mut has_image = false;
    let mut has_other = false;
    let mut has_warning_link = false;
    let mut has_required_features = false;
    let mut foreign_range: Option<(usize, usize)> = None;
    let mut foreign_start_idx: Option<usize> = None;
    let mut foreign_capture_depth: Option<usize> = None;

    let end = loop {
        match reader.read_event_into(&mut buf)? {
            Event::Start(e) => {
                let local = e.local_name();
                if start_has_required_features(&e)? {
                    has_required_features = true;
                }
                if depth == 0 {
                    match local.as_ref() {
                        b"foreignObject" => {
                            has_foreign = true;
                        }
                        b"image" => {
                            has_image = true;
                        }
                        _ => {
                            has_other = true;
                        }
                    }
                    if local.as_ref() == b"a" && start_has_warning_link(&e)? {
                        has_warning_link = true;
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
                if start_has_required_features(&e)? {
                    has_required_features = true;
                }
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
                        b"image" => {
                            has_image = true;
                        }
                        _ => {
                            has_other = true;
                        }
                    }
                    if local.as_ref() == b"a" && start_has_warning_link(&e)? {
                        has_warning_link = true;
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
            Event::Text(e) => {
                if depth == 0 && !e.as_ref().iter().all(u8::is_ascii_whitespace) {
                    has_other = true;
                }
                events.push(Event::Text(e.into_owned()));
            }
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
        has_image,
        has_other,
        has_warning_link,
        has_required_features,
    })
}

fn round_decimal_str(value: f64) -> String {
    let mut s = format!("{value:.6}");
    if let Some(dot) = s.find('.') {
        while s.ends_with('0') {
            s.pop();
        }
        if s.ends_with('.') {
            s.truncate(dot);
        }
    }
    if s == "-0" {
        s = "0".to_string();
    }
    s
}

fn round_decimals_in_str(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out = String::with_capacity(input.len());
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i] as char;
        let is_sign = c == '-' || c == '+';
        let is_digit = c.is_ascii_digit();
        let is_dot = c == '.';
        let start_number = is_digit
            || (is_dot && i + 1 < bytes.len() && (bytes[i + 1] as char).is_ascii_digit())
            || (is_sign
                && i + 1 < bytes.len()
                && ((bytes[i + 1] as char).is_ascii_digit() || bytes[i + 1] as char == '.'));
        if !start_number {
            out.push(c);
            i += 1;
            continue;
        }

        let start = i;
        if is_sign {
            i += 1;
        }
        let mut digits_before = 0usize;
        while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
            digits_before += 1;
            i += 1;
        }
        let mut saw_dot = false;
        let mut digits_after = 0usize;
        if i < bytes.len() && bytes[i] as char == '.' {
            saw_dot = true;
            i += 1;
            while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                digits_after += 1;
                i += 1;
            }
        }

        if saw_dot && digits_after > 0 {
            let token = &input[start..i];
            if let Ok(value) = token.parse::<f64>() {
                out.push_str(&round_decimal_str(value));
            } else {
                out.push_str(token);
            }
        } else {
            out.push_str(&input[start..i]);
        }
    }
    out
}

fn round_event_start(start: &BytesStart<'_>) -> BytesStart<'static> {
    let name = String::from_utf8_lossy(start.name().as_ref()).to_string();
    let mut new_start = BytesStart::new(name);
    for attr in start.attributes().with_checks(false) {
        let attr = attr.unwrap_or_else(|err| panic!("Failed to parse attributes: {err}"));
        let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
        let value = attr
            .unescape_value()
            .unwrap_or_else(|err| panic!("Failed to parse attributes: {err}"))
            .into_owned();
        let rounded = round_decimals_in_str(&value);
        new_start.push_attribute((key.as_str(), rounded.as_str()));
    }
    new_start
}

fn round_event(event: Event<'_>) -> Event<'static> {
    match event {
        Event::Start(e) => Event::Start(round_event_start(&e)),
        Event::Empty(e) => Event::Empty(round_event_start(&e)),
        Event::Text(e) => {
            let value = e
                .decode()
                .unwrap_or_else(|err| panic!("Failed to parse text: {err}"))
                .into_owned();
            let rounded = round_decimals_in_str(&value);
            Event::Text(BytesText::from_escaped(rounded))
        }
        other => other.into_owned(),
    }
}

pub fn clean_svg(content: &str) -> String {
    let mut reader = Reader::from_str(content);
    reader.config_mut().trim_text(false);
    let mut writer = Writer::new(Vec::new());
    let mut buf = Vec::new();
    let mut required_features_used = false;
    let mut warning_written = false;

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Eof) => break,
            Ok(Event::Start(e)) if e.local_name().as_ref() == b"switch" => {
                let switch_content = read_switch(&mut reader)
                    .unwrap_or_else(|err| panic!("Failed to parse switch: {err}"));
                if switch_content.has_required_features {
                    required_features_used = true;
                }
                if let Some((start, end)) = switch_content.foreign_range.filter(|_| {
                    switch_content.has_foreign
                        && switch_content.has_image
                        && !switch_content.has_other
                }) {
                    for event in switch_content
                        .events
                        .into_iter()
                        .skip(start)
                        .take(end - start)
                    {
                        writer
                            .write_event(round_event(event))
                            .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
                    }
                } else if switch_content.has_warning_link
                    && !switch_content.has_foreign
                    && !switch_content.has_image
                {
                } else {
                    writer
                        .write_event(Event::Start(round_event_start(&e)))
                        .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
                    for event in switch_content.events {
                        writer
                            .write_event(round_event(event))
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
            Ok(Event::Start(e)) => {
                if start_has_required_features(&e)
                    .unwrap_or_else(|err| panic!("Failed to parse attributes: {err}"))
                {
                    required_features_used = true;
                }
                writer
                    .write_event(Event::Start(round_event_start(&e)))
                    .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
            }
            Ok(Event::Empty(e)) => {
                if start_has_required_features(&e)
                    .unwrap_or_else(|err| panic!("Failed to parse attributes: {err}"))
                {
                    required_features_used = true;
                }
                writer
                    .write_event(Event::Empty(round_event_start(&e)))
                    .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
            }
            Ok(Event::End(e)) if e.local_name().as_ref() == b"svg" => {
                if required_features_used && !warning_written {
                    writer
                        .get_mut()
                        .extend_from_slice(WARNING_SWITCH.as_bytes());
                    warning_written = true;
                }
                writer
                    .write_event(Event::End(e.into_owned()))
                    .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
            }
            Ok(event) => {
                writer
                    .write_event(round_event(event))
                    .unwrap_or_else(|err| panic!("Failed to write SVG: {err}"));
            }
            Err(err) => panic!("Failed to parse SVG: {err}"),
        }
        buf.clear();
    }

    String::from_utf8(writer.into_inner()).expect("cleaned SVG must be UTF-8")
}
