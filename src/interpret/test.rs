use super::*;

fn print_result() -> bool {
    true
}

fn test_consult(s: &str) {
    let text = consult::test::consult(s).unwrap();

    let ctx = &mut Context {
        procedures: text.procedures,
    };
    for (t, v) in text.initialization {
        solve::eval(ctx, &t, &v, print_result);
    }
}

#[test]
fn test() {
    test_consult("./test/vanilla/vanilla.pl");
}
