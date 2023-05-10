use super::*;
use term::*;

pub(crate) fn read_term(s: &str) -> (Term, Vec<VarInfo>) {
    let mut stream = utf8reader::Utf8Reader::new(s.as_bytes(), format!("{{{}}}", s).as_str());
    let mut var_info = Vec::new();

    (
        parser::next(&Context::default(), &mut var_info, &mut stream)
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
