use super::*;

#[test]
fn test_read_term() {
    test_term("test_false((1 \\= 1)).");
    test_term("test_false((\\+ (true))).");
    test_term("defined((\\+)/1).");
    test_term("/(7,35).");
    test_term("X21 is /(7,35).");
    test_term("0.2000.");
    test_term("test_val(X21 is /(7,35),X21,0.2000).");
}

fn test_term(s: &str) {
    let ctx: Context = Default::default();
    let mut stream = utf8reader::Utf8Reader::new(s.as_bytes(), s);

    let r = parser::next(&ctx, &mut stream, false);
    if let Err(e) = r {
        println!("{:?}", e);
    }
}
