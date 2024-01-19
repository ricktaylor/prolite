use std::thread;

use super::*;

fn print_result(t: &Rc<read_term::Term>, _: &[read_term::VarInfo]) -> bool {
    match &t.kind {
        read_term::TermKind::Atom(s) => eprintln!("initialization {} ... ok", s),
        read_term::TermKind::Compound(c) => {
            eprintln!("initialization {}/{} ... ok", c.functor, c.args.len())
        }
        _ => unreachable!(),
    }
    false
}

fn test_consult(s: &str) {
    let text = consult::test::consult(s).unwrap();

    let ctx = &mut Context {
        procedures: text.procedures,
        char_conversion: text.char_conversion,
        flags: text.flags,
        operators: text.operators,
    };
    for (t, v) in text.initialization {
        solve::eval(ctx, t.clone(), || print_result(&t, &v));
    }
    solve::eval(
        ctx,
        read_term::Term::new_atom("validate".to_string(), None),
        || true,
    );
}

#[test]
fn test() {
    let child = thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(move || {
            test_consult("./test/vanilla/vanilla.pl");
        })
        .unwrap();

    child.join().unwrap();
}
