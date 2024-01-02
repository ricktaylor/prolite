use super::*;
use solve::Solver;
use term::*;

pub(super) fn solve_write(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    let mut term = if args.len() == 2 { &args[1] } else { &args[0] };
    while let term::TermKind::Var(idx) = &term.kind {
        match &substs[*idx] {
            None => break,
            Some(t) => term = t,
        }
    }
    print!("{:?}", *term);
    next.solve(ctx, substs)
}

pub(super) fn solve_nl(
    ctx: &mut Context,
    _: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    println!();
    next.solve(ctx, substs)
}
