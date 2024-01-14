use super::*;
use std::rc::Rc;
use term::*;

pub(crate) fn read_term(s: &str) -> (Rc<Term>, Vec<VarInfo>) {
    let mut var_info = Vec::new();
    (
        parser::next(
            Context {
                flags: &mut flags::Flags::default(),
                operators: &mut &operators::Operator::default_table(),
                char_conversion: &mut HashMap::new(),
                greedy: false,
            },
            &mut var_info,
            &mut utf8reader::Utf8Reader::new(s.as_bytes(), format!("{{{}}}", s).as_str()),
        )
        .unwrap()
        .unwrap(),
        var_info,
    )
}

#[test]
fn test() {
    read_term("test_false((1 \\= 1)).");
    read_term("test_false((\\+ (true))).");
    read_term("defined((\\+)/1).");
    read_term("/(7,35).");
    read_term("X21 is /(7,35).");
    read_term("0.2000.");
    read_term("test_val(X21 is /(7,35),X21,0.2000).");
}
