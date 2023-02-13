use std::collections::HashMap;
use std::vec::Vec;

#[allow(non_camel_case_types)]
pub enum OperatorSpecifier {
    fx,
    fy,
    xfx,
    xfy,
    yfx,
    xf,
    yf
}

pub struct Operator
{
    specifier: OperatorSpecifier,
    precedence: u16
}

pub type OperatorTable = HashMap<String,Vec<Operator>>;

impl Operator {
    pub fn default_table() -> OperatorTable {
        OperatorTable::from([
            (":-".to_string(),vec![Operator{specifier: OperatorSpecifier::fx,precedence: 1200},Operator{specifier: OperatorSpecifier::xfx,precedence: 1200}]),
            ("-->".to_string(),vec![Operator{specifier: OperatorSpecifier::xfx,precedence: 1200}]),
            ("?-".to_string(),vec![Operator{specifier: OperatorSpecifier::fx,precedence: 1200}]),
            (";".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 1100}]),
            ("->".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 1050}]),               
            (",".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 1000}]),
            ("\\+".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 900}]),
            ("=".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("<".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            (">".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("\\=".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("==".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("=<".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("@<".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("@>".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("is".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            (">=".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("\\==".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("@=<".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("@>=".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("=..".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("=:=".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            ("=\\=".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 700}]),
            (":".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 600}]),
            ("+".to_string(),vec![Operator{specifier: OperatorSpecifier::fy,precedence: 200},Operator{specifier: OperatorSpecifier::yfx,precedence: 500}]),
            ("-".to_string(),vec![Operator{specifier: OperatorSpecifier::fy,precedence: 200},Operator{specifier: OperatorSpecifier::yfx,precedence: 500}]),
            ("\\/".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 500}]),
            ("/\\".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 500}]),
            ("*".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("/".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            (">>".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("//".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("<<".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("rem".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("mod".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("div".to_string(),vec![Operator{specifier: OperatorSpecifier::yfx,precedence: 400}]),
            ("**".to_string(),vec![Operator{specifier: OperatorSpecifier::xfx,precedence: 200}]),
            ("^".to_string(),vec![Operator{specifier: OperatorSpecifier::xfy,precedence: 200}]),
            ("\\".to_string(),vec![Operator{specifier: OperatorSpecifier::fy,precedence: 200}]),
        ])
    }

}

pub fn lookup_op<'a>(table: &'a OperatorTable, name: &str) -> Option<&'a Operator> {
    let r= table.get(name)?;
    for o in r.iter() {
        match o.specifier {
            OperatorSpecifier::fx | OperatorSpecifier::fy => continue,
            _ => return Some(o),
        }
    }
    r.first()
}

pub fn lookup_prefix_op<'a>(table: &'a OperatorTable, name: &str) -> Option<&'a Operator> {
    let r= table.get(name)?;
    for o in r.iter() {
        match o.specifier {
            OperatorSpecifier::fx | OperatorSpecifier::fy => return Some(o),
            _ => continue,
        }
    }
    r.first()
}
