use super::*;
use term::*;

fn renamed_copy<'a>(t: &'a Rc<Term>, substs: &[Var<'a>], new_substs: &mut Vec<Var>) -> Rc<Term> {
    match &t.kind {
        TermKind::Var(idx) => {
            if let Some(t) = substs[*idx] {
                renamed_copy(t, substs, new_substs)
            } else {
                new_substs.push(None);
                Rc::new(Term {
                    kind: TermKind::Var(new_substs.len() - 1),
                    location: t.location.clone(),
                })
            }
        }
        TermKind::Compound(c) => Term::new_compound(
            c.functor.clone(),
            t.location.clone(),
            c.args
                .iter()
                .map(|t| renamed_copy(t, substs, new_substs))
                .collect(),
        ),
        _ => t.clone(),
    }
}

pub(super) fn solve_findall(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    let mut new_substs: Vec<Var> = Vec::new();
    let mut solutions: Vec<Rc<Term>> = Vec::new();

    solve::call(
        ctx,
        &args[1],
        substs,
        &mut Continuation::new(|_, substs| {
            solutions.push(renamed_copy(&args[0], substs, &mut new_substs));
            Response::Fail
        }),
    )
    .map_failed(|| {
        let l = solutions.into_iter().rev().fold(
            Term::new_atom("[]".to_string(), stream::Span::default()),
            |list, t| Term::new_compound(".".to_string(), stream::Span::default(), vec![t, list]),
        );

        solve::unify(&l, &args[2], new_substs).map_or_else(|r| r, |substs| next.solve(ctx, &substs))
    })
}

/*pub(super) fn solve_setof(
    ctx: &mut Context,
    args: &[Term],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {

    let witness = witness_set(&args[0],&args[1],substs);
    let goal = iterated_goal(&args[1]);

    let mut new_substs: Vec<Var> = Vec::new();
    let mut solutions: Vec<(Term,Term)> = Vec::new();

    solve::call(
        ctx,
        &goal,
        substs,
        &mut Continuation::new(|_, substs| {
            solutions.push((renamed_copy(&witness, substs, &mut new_substs),renamed_copy(&args[0], substs, &mut new_substs)));
            Response::Fail
        }),
    ).map_failed(|| {
        while !solutions.is_empty() {

        }

        Response::Fail
    })
}*/
