use super::*;
use builtins::*;
use term::*;
use user_defined::*;

pub(super) fn solve(
    ctx: &mut Context,
    goal: &Term,
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    match &goal.kind {
        TermKind::Atom(s) => match is_builtin(&format!("{}/0", s)) {
            Some(f) => (f)(ctx, &[], substs, next),
            None => user_defined::solve(ctx, goal, substs, next),
        },
        TermKind::Compound(c) => match is_builtin(&format!("{}/{}", c.functor, c.args.len())) {
            Some(f) => (f)(ctx, &c.args, substs, next),
            None => user_defined::solve(ctx, goal, substs, next),
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

pub(crate) fn eval<F: FnMut(&[Var]) -> bool>(
    ctx: &mut Context,
    goal: &Term,
    var_info: &[VarInfo],
    callback: F,
) -> Response {
    solve(
        ctx,
        goal,
        &vec![Var::default(); var_info.len()],
        &mut CallbackSolver { callback },
    )
}
