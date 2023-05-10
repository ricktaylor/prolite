use super::*;

fn print_result(_: &[Var]) -> bool {
    true
}

fn test_eval(s: &str) {
    let (t, v) = crate::read_term::test::read_term(s);
    let r = solve::eval(&mut Context::default(), &t, &v, print_result);

    println!("{:?}", r);
}

fn test_consult(s: &str) {
    let text = consult::test::consult(s).unwrap();

    let mut ctx = Context {
        procedures: text.procedures,
    };
    for (t, v) in text.initialization {
        solve::eval(&mut ctx, &t, &v, print_result);
    }
}

#[test]
fn test() {
    //test_eval("hello.");

    test_consult("./test/vanilla/vanilla.pl");
}
