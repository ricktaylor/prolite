use std::todo;

use super::*;
use builtins::*;
use term::*;

pub(super) fn solve(
    ctx: &mut Context,
    goal: &Rc<Term>,
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    match &goal.kind {
        TermKind::Var(idx) => call(ctx, goal, substs, next),
        TermKind::Atom(s) => match is_builtin(&format!("{}/0", s)) {
            Some(f) => (f)(ctx, &mut [], substs, next),
            None => user_defined::solve(ctx, goal, substs, next),
        },
        TermKind::Compound(c) => match is_builtin(&format!("{}/{}", c.functor, c.args.len())) {
            Some(f) => (f)(ctx, &c.args, substs, next),
            None => user_defined::solve(ctx, goal, substs, next),
        },
        _ => todo!(),
    }
}

pub(super) fn call<'a>(
    ctx: &mut Context,
    mut goal: &'a Rc<Term>,
    substs: &[Var<'a>],
    next: &mut dyn Solver,
) -> Response {
    // Dereference goal
    while let term::TermKind::Var(idx) = &goal.kind {
        if let Some(t) = substs[*idx] {
            goal = t;
        } else {
            // Instantiation error!
            todo!()
        }
    }

    // call/1 is "not transparent" to cut, so use a continuation to record the response from next
    let mut next_cut = false;
    match solve(
        ctx,
        goal,
        substs,
        &mut Continuation::new(|ctx, substs| {
            next.solve(ctx, substs).map_cut(|| {
                next_cut = true;
                Response::Cut
            })
        }),
    ) {
        Response::Cut if !next_cut => Response::Fail,
        /*Response::Throw(_) => {
            // Catch callable error!
        }*/
        r => r,
    }
}

pub(super) fn unify<'a>(
    a: &'a Rc<Term>,
    b: &'a Rc<Term>,
    mut substs: Vec<Var<'a>>,
) -> Result<Vec<Var<'a>>, Response> {
    match &a.kind {
        TermKind::Var(idx) => {
            if let Some(a) = substs[*idx] {
                return unify(a, b, substs);
            }
            substs[*idx] = Some(b);
        }
        TermKind::Integer(i1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs[*idx] {
                    return unify(a, b, substs);
                }
                substs[*idx] = Some(a)
            }
            TermKind::Integer(i2) if *i1 == *i2 => {}
            TermKind::Float(f) if *i1 as f64 == *f => {}
            _ => return Err(Response::Fail),
        },
        TermKind::Float(f1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs[*idx] {
                    return unify(a, b, substs);
                }
                substs[*idx] = Some(a)
            }
            TermKind::Float(f2) if *f1 == *f2 => {}
            TermKind::Integer(i) if *f1 == *i as f64 => {}
            _ => return Err(Response::Fail),
        },
        TermKind::Atom(s1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs[*idx] {
                    return unify(a, b, substs);
                }
                substs[*idx] = Some(a)
            }
            TermKind::Atom(s2) if *s1 == *s2 => {}
            _ => return Err(Response::Fail),
        },
        TermKind::Compound(c1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs[*idx] {
                    return unify(a, b, substs);
                }
                substs[*idx] = Some(a)
            }
            TermKind::Compound(c2)
                if c1.functor == c2.functor && c1.args.len() == c2.args.len() =>
            {
                return c1
                    .args
                    .iter()
                    .zip(&c2.args)
                    .try_fold(substs, |substs, (a, b)| unify(a, b, substs));
            }
            _ => return Err(Response::Fail),
        },
    };
    Ok(substs)
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
    goal: &Rc<Term>,
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
