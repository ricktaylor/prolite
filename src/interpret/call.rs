use super::*;

use builtins::*;
use term::*;

#[derive(Clone)]
struct ClosureSolver<F: Fn(&mut Context, &[&term::Term], &mut [Var], &dyn Solver) -> Response> {
    closure: F,
}

impl<F> Solver for ClosureSolver<F>
where
    F: Fn(&mut Context, &[&term::Term], &mut [Var], &dyn Solver) -> Response,
{
    fn call(
        &self,
        ctx: &mut Context,
        args: &[&term::Term],
        substs: &mut [Var],
        next: &dyn Solver,
    ) -> Response {
        (self.closure)(ctx, args, substs, next)
    }
}

fn user_defined(
    ctx: &mut Context,
    args: &[&term::Term],
    substs: &mut [Var],
    next: &dyn Solver,
) -> Response {
    todo!()
}

pub(super) fn call(
    ctx: &mut Context,
    args: &[&term::Term],
    substs: &mut [Var],
    next: &dyn Solver,
) -> Response {
    match &args[0].kind {
        TermKind::Atom(s) => match is_builtin(&format!("{}/0", s)) {
            Some(f) => (f)(ctx, &[], substs, next),
            None => user_defined(ctx, args, substs, next),
        },
        TermKind::Compound(c) => match is_builtin(&format!("{}/{}", c.functor, c.args.len())) {
            Some(f) => {
                let mut a = Vec::with_capacity(c.args.len());
                for t in c.args.iter() {
                    a.push(t);
                }
                (f)(ctx, &a, substs, next)
            }
            None => user_defined(ctx, args, substs, next),
        },
        TermKind::Var(s) => substs.binary_search_by(|v| v.name.cmp(s)).map_or_else(
            |_| panic!(),
            |idx| {
                substs[idx]
                    .value
                    .map_or_else(|| Response::Throw, |t| call(ctx, &[t], substs, next))
            },
        ),
        _ => Response::Throw,
    }
}
