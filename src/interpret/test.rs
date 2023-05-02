use super::*;

fn print_result(_: &[Var]) -> bool {
    true
}

fn test_eval(s: &str) {
    let t = crate::read_term::test::read_term(s);
    let r = solve::eval(&t, print_result);

    println!("{:?}", r);
}

fn test_consult(s: &str) {

    let t = consult::test::consult(s).unwrap();
    for i in t.initialization {
        println!("{:?}", solve::eval(&i, print_result));
    }

}

#[test]
fn test() {
    //test_eval("hello.");

    test_consult("./test/vanilla/vanilla.pl");
}
