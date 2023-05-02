use super::*;

use builtins::*;
use term::*;

fn user_defined(
    ctx: &mut Context,
    goal: &term::Term,
    substs: &[Var],
    next: &dyn Solver,
) -> Response {

    match &goal.kind {
        TermKind::Integer(_) => todo!(),
        TermKind::Float(_) => todo!(),
        TermKind::Var(_) => todo!(),
        TermKind::Atom(s) => println!("User defined function: {}/0 at {}:{}:{}",s,goal.location.start.source,goal.location.start.line,goal.location.start.column),
        TermKind::Compound(c) => println!("User defined function: {}/{} at {}:{}:{}",c.functor,c.args.len(),goal.location.start.source,goal.location.start.line,goal.location.start.column),
    }

    todo!()
}

pub(super) fn solve(
    ctx: &mut Context,
    goal: &term::Term,
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    match &goal.kind {
        TermKind::Atom(s) => match is_builtin(&format!("{}/0", s)) {
            Some(f) => (f)(ctx, &[], substs, next),
            None => user_defined(ctx, goal, substs, next),
        },
        TermKind::Compound(c) => match is_builtin(&format!("{}/{}", c.functor, c.args.len())) {
            Some(f) => (f)(ctx, &c.args, substs, next),
            None => user_defined(ctx, goal, substs, next),
        },
        _ => todo!(),
    }
}

struct CallbackSolver<F: FnMut(&[Var]) -> bool> {
    callback: F,
}

impl<F> Solver for CallbackSolver<F>
where
    F: FnMut(&[Var]) -> bool,
{
    fn solve(&mut self, _: &mut Context, substs: &[Var]) -> Response {
        if (self.callback)(substs) {
            Response::Fail
        } else {
            Response::Cut
        }
    }
}

pub(crate) fn eval<F: FnMut(&[Var]) -> bool>(goal: &term::Term, callback: F) -> Response {
    // Todo: Actually pull out vars!!
    let substs = Vec::new();

    solve::solve(
        &mut Context::default(),
        goal,
        &substs,
        &mut CallbackSolver { callback },
    )
}