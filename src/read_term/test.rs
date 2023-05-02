use super::*;
use term::*;

pub(crate) fn read_term(s: &str) -> Term {
    let ctx = Context::default();
    let mut stream = utf8reader::Utf8Reader::new(s.as_bytes(), s);

    parser::next(&ctx, &mut stream, false).unwrap().unwrap()
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
