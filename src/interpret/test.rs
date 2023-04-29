use super::*;

fn print_result(_: &[Var]) -> bool {
    true
}

fn test_eval(s: &str) {
    let t = crate::read_term::test::read_term(s);
    let r = solve::eval(&t, print_result);

    println!("{:?}", r);
}

#[test]
fn test_interpret() {
    test_eval("hello.");
}
