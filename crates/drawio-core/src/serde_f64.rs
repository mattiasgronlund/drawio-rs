use serde::{Deserialize, Deserializer, Serializer};

#[derive(Debug, thiserror::Error)]
pub enum SerdeF64Error {
    #[error("invalid floating point value for JSON (NaN/Infinity): {0}")]
    NonFinite(f64),
}

pub fn ser_f64_compact<S>(v: &f64, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    if !v.is_finite() {
        return Err(serde::ser::Error::custom(format!(
            "non-finite float not allowed in JSON: {v}"
        )));
    }

    // Normalize -0.0 -> 0
    let v = if *v == 0.0 { 0.0 } else { *v };

    // If it's an integer, serialize as an integer JSON number (no ".0")
    if v.fract() == 0.0 {
        // Try i64 first (covers typical draw.io values)
        if v >= i64::MIN as f64 && v <= i64::MAX as f64 {
            return s.serialize_i64(v as i64);
        }
        // Otherwise fall back to f64 (still valid JSON)
    }

    s.serialize_f64(v)
}

pub fn ser_opt_f64_compact<S>(v: &Option<f64>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match v {
        None => s.serialize_none(),
        Some(x) => ser_f64_compact(x, s),
    }
}

pub fn de_f64<'de, D>(d: D) -> Result<f64, D::Error>
where
    D: Deserializer<'de>,
{
    // Accept both integer and float JSON numbers.
    // serde will coerce JSON number into f64 here.
    let v = f64::deserialize(d)?;
    if !v.is_finite() {
        return Err(serde::de::Error::custom(format!(
            "non-finite float not allowed in JSON: {v}"
        )));
    }
    Ok(if v == 0.0 { 0.0 } else { v })
}

pub fn de_opt_f64<'de, D>(d: D) -> Result<Option<f64>, D::Error>
where
    D: Deserializer<'de>,
{
    let v = Option::<f64>::deserialize(d)?;
    if let Some(x) = v {
        if !x.is_finite() {
            return Err(serde::de::Error::custom(format!(
                "non-finite float not allowed in JSON: {x}"
            )));
        }
        Ok(Some(if x == 0.0 { 0.0 } else { x }))
    } else {
        Ok(None)
    }
}
