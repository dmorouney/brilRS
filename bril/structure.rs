use serde::{Deserialize, Serialize};
use serde_json::Value;
use::std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilProgram {
    pub functions: Vec<BrilFunction>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilFunction { 
    pub name: String,
    pub args: Option<Vec<BrilArg>>,
    #[serde(rename = "type")]
    pub type_: Option<BrilType>,
    pub instrs: Vec<BrilInstruction>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum BrilType {
    SimpleType(String),
    ParameterizedType { parameter:String, type_:Box<BrilType> },
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilArg {
    pub name: String,
    #[serde(rename = "type")]
    pub type_: BrilType,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum BrilInstruction {
    Operation(BrilOperation),
    Label(BrilLabel),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum BrilOperation {
    Constant(BrilConstant),
    Value(BrilValue),
    Effect(BrilEffect),
}

#[allow(dead_code)]
impl BrilOperation {
    pub fn as_value(&self) -> Option<&BrilValue> {
        if let BrilOperation::Value(value) = self {
            Some(value)
        } else {
            None
        }
    }

    pub fn as_constant(&self) -> Option<&BrilConstant> {
        if let BrilOperation::Constant(constant) = self {
            Some(constant)
        } else {
            None
        }
    }

    pub fn as_effect(&self) -> Option<&BrilEffect> {
        if let BrilOperation::Effect(effect) = self {
            Some(effect)
        } else {
            None
        }
    }
    
    pub fn get_dest(&self) -> Option<String> {
        match self {
            BrilOperation::Effect(_) => None,
            BrilOperation::Constant(BrilConstant {dest, .. }) | 
            BrilOperation::Value(BrilValue {dest, .. }) => Some(dest.clone()) 
        }
    }

    pub fn get_args(&self) -> Option<Vec<String>> {
        match self {
            BrilOperation::Constant(_) => None,
            BrilOperation::Effect(BrilEffect { args, ..}) | 
            BrilOperation::Value(BrilValue {args, .. }) => args.clone(),
        }
    }
    
    pub fn get_type(&self) -> Option<String> {
        match self {
            BrilOperation::Effect(_) => None,
            BrilOperation::Constant(BrilConstant {type_, .. }) | 
            BrilOperation::Value(BrilValue {type_, .. }) => {
                match type_ {
                    BrilType::SimpleType(t) => Some(t.clone()),
                    BrilType::ParameterizedType { parameter: p, type_: t } => Some(format!("{p}<{:?}>", t.clone())),
                }
            }
        }
    }
    
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilLabel {
    pub label: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilConstant {
    pub op: String,
    pub dest: String,
    #[serde(rename = "type")]
    pub type_: BrilType,
    pub value: Value,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilValue {
    pub op: String,
    pub dest: String,
    #[serde(rename = "type")]
    pub type_: BrilType,
    pub args: Option<Vec<String>>,
    pub funcs: Option<Vec<String>>,
    pub labels: Option<Vec<String>>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BrilEffect {
    pub op: String,
    pub args: Option<Vec<String>>, 
    pub funcs: Option<Vec<String>>,
    pub labels: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct BrilBlock {
    pub instructions: Vec<BrilInstruction>,
    pub label: String,
    pub next: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct CFGFunction {
    pub name: String,
    pub blocks: HashMap<String, Vec<BrilOperation>>,
    pub edges: HashMap<String, Vec<String>>,
    pub order: Vec<String>,
    pub args: Vec<String>,
}

// Local Value Numbering Table.: 
#[derive(Eq, Hash, PartialEq, Debug, Clone)]
pub enum LVNValue {
    Const(LVal),
    Op(String, Vec<usize>),
}
#[derive(Eq, Hash, PartialEq, Debug, Clone)]
pub enum LVal {
    Bool(bool),
    Int(i64),
}

#[derive(Debug, Clone)]
pub struct LVNRow {
    pub idx: usize,
    pub value: LVNValue,
    pub var: String,
    pub typ: String,
}
#[derive(Debug, Clone)]
pub struct LVNTable {
    pub rows: Vec<LVNRow>,
    pub env: HashMap<String, usize>,
}

