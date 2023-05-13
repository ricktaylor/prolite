use super::*;
use builtins::*;
use term::*;

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

pub(super) fn call(ctx: &mut Context, t: &Term, substs: &[Var], next: &mut dyn Solver) -> Response {
    // call/1 is "not transparent" to cut, so use a continuation to record the response from next
    let mut next_cut = false;
    let close_cut = &mut Continuation::new(|ctx, substs| {
        next.solve(ctx, substs).map_cut(|| {
            next_cut = true;
            Response::Cut
        })
    });

    if let TermKind::Var(_) = &t.kind {
        // Convert variable to body
        solve(
            ctx,
            &deref_var(t, substs).clone().into_goal(),
            substs,
            close_cut,
        )
    } else {
        solve(ctx, t, substs, close_cut)
    }
    .map_cut(|| {
        if next_cut {
            Response::Cut
        } else {
            Response::Fail
        }
    })
}

pub(super) fn unify<'a>(
    a: &'a Term,
    b: &'a Term,
    mut substs: Vec<Var<'a>>,
) -> Result<Vec<Var<'a>>, Response> {
    match &a.kind {
        TermKind::Var(idx) => substs[*idx] = Some(b),
        TermKind::Integer(i1) => match &b.kind {
            TermKind::Var(idx) => substs[*idx] = Some(a),
            TermKind::Integer(i2) if *i1 == *i2 => {}
            TermKind::Float(f) if *i1 as f64 == *f => {}
            _ => return Err(Response::Fail),
        },
        TermKind::Float(f1) => match &b.kind {
            TermKind::Var(idx) => substs[*idx] = Some(a),
            TermKind::Float(f2) if *f1 == *f2 => {}
            TermKind::Integer(i) if *f1 == *i as f64 => {}
            _ => return Err(Response::Fail),
        },
        TermKind::Atom(s1) => match &b.kind {
            TermKind::Var(idx) => substs[*idx] = Some(a),
            TermKind::Atom(s2) if *s1 == *s2 => {}
            _ => return Err(Response::Fail),
        },
        TermKind::Compound(c1) => match &b.kind {
            TermKind::Var(idx) => substs[*idx] = Some(a),
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
