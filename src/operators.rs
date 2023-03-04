use std::collections::HashMap;
use std::vec::Vec;

#[allow(non_camel_case_types)]
#[derive(Debug,Clone)]
pub(crate) enum Operator {
    fx(u16),
    fy(u16),
    xfx(u16),
    xfy(u16),
    yfx(u16),
    xf(u16),
    yf(u16)
}

pub(crate) type OperatorTable = HashMap<String,Vec<Operator>>;

impl Operator {
    pub(crate) fn default_table() -> OperatorTable {
        OperatorTable::from([
            (":-".to_string(),vec![Operator::fx(1200),Operator::xfx(1200)]),
            ("-->".to_string(),vec![Operator::xfx(1200)]),
            ("?-".to_string(),vec![Operator::fx(1200)]),
            (";".to_string(),vec![Operator::xfy(1100)]),
            ("->".to_string(),vec![Operator::xfy(1050)]),
            (",".to_string(),vec![Operator::xfy(1000)]),
            ("\\+".to_string(),vec![Operator::xfy(900)]),
            ("=".to_string(),vec![Operator::xfy(700)]),
            ("<".to_string(),vec![Operator::xfy(700)]),
            (">".to_string(),vec![Operator::xfy(700)]),
            ("\\=".to_string(),vec![Operator::xfy(700)]),
            ("==".to_string(),vec![Operator::xfy(700)]),
            ("=<".to_string(),vec![Operator::xfy(700)]),
            ("@<".to_string(),vec![Operator::xfy(700)]),
            ("@>".to_string(),vec![Operator::xfy(700)]),
            ("is".to_string(),vec![Operator::xfy(700)]),
            (">=".to_string(),vec![Operator::xfy(700)]),
            ("\\==".to_string(),vec![Operator::xfy(700)]),
            ("@=<".to_string(),vec![Operator::xfy(700)]),
            ("@>=".to_string(),vec![Operator::xfy(700)]),
            ("=..".to_string(),vec![Operator::xfy(700)]),
            ("=:=".to_string(),vec![Operator::xfy(700)]),
            ("=\\=".to_string(),vec![Operator::xfy(700)]),
            (":".to_string(),vec![Operator::xfy(600)]),
            ("+".to_string(),vec![Operator::fy(200),Operator::yfx(500)]),
            ("-".to_string(),vec![Operator::fy(200),Operator::yfx(500)]),
            ("\\/".to_string(),vec![Operator::yfx(500)]),
            ("/\\".to_string(),vec![Operator::yfx(500)]),
            ("*".to_string(),vec![Operator::yfx(400)]),
            ("/".to_string(),vec![Operator::yfx(400)]),
            (">>".to_string(),vec![Operator::yfx(400)]),
            ("//".to_string(),vec![Operator::yfx(400)]),
            ("<<".to_string(),vec![Operator::yfx(400)]),
            ("rem".to_string(),vec![Operator::yfx(400)]),
            ("mod".to_string(),vec![Operator::yfx(400)]),
            ("div".to_string(),vec![Operator::yfx(400)]),
            ("**".to_string(),vec![Operator::xfx(200)]),
            ("^".to_string(),vec![Operator::xfy(200)]),
            ("\\".to_string(),vec![Operator::fy(200)]),
        ])
    }

}
