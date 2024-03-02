use std::path::Path;
use std::thread;

use super::*;

fn test_consult(s: &str, goal: &str) {
    let mut ctx = Context::consult(
        &mut consult::test::FSResolver::new(Path::new(s).parent().unwrap().to_str().unwrap())
            .unwrap(),
        s,
    )
    .unwrap();

    ctx.eval(goal, || true);
}

#[test]
fn test() {
    test_consult("./test/vanilla/vanilla.pl", "validate");

    let child = thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(move || {
            test_consult("./test/vanilla/vanilla.pl", "validate");
            test_consult("./test/inriasuite/inriasuite.pl", "run_all_tests");
        })
        .unwrap();

    child.join().unwrap();
}
