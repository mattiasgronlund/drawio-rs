use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::serde_f64;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MxFile {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub host: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub file_type: Option<String>,

    #[serde(default)]
    pub diagrams: Vec<Diagram>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Diagram {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// Raw text inside `<diagram>...</diagram>` if it is encoded/compressed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub encoded_payload: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub graph_model: Option<MxGraphModel>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MxGraphModel {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub dx: Option<i64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub dy: Option<i64>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub grid: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub grid_size: Option<i64>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub guides: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tooltips: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub connect: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub arrows: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub fold: Option<bool>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub page: Option<bool>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub page_scale: Option<f64>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub page_width: Option<f64>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub page_height: Option<f64>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub math: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub shadow: Option<bool>,

    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extra: BTreeMap<String, String>,

    #[serde(default)]
    pub root: Root,

    #[serde(skip)]
    pub user_objects: Vec<MxCell>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Root {
    #[serde(default)]
    pub cells: Vec<MxCell>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MxCell {
    pub id: String,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub parent: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub style: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub vertex: Option<bool>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub edge: Option<bool>,

    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extra: BTreeMap<String, String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub geometry: Option<MxGeometry>,

    #[serde(default, skip)]
    pub order: usize,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MxGeometry {
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub x: Option<f64>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub y: Option<f64>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub width: Option<f64>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub height: Option<f64>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub relative: Option<bool>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub as_attr: Option<String>,

    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub extra: BTreeMap<String, String>,

    #[serde(skip)]
    pub source_point: Option<MxPoint>,

    #[serde(skip)]
    pub target_point: Option<MxPoint>,

    #[serde(skip)]
    pub offset_point: Option<MxPoint>,

    #[serde(skip)]
    pub points: Vec<MxPoint>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MxPoint {
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub x: Option<f64>,

    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        serialize_with = "serde_f64::ser_opt_f64_compact",
        deserialize_with = "serde_f64::de_opt_f64"
    )]
    pub y: Option<f64>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub as_attr: Option<String>,
}
