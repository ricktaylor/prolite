use super::*;
use term::*;

fn renamed_copy(t: &Term, substs: &[Var], new_substs: &mut Vec<Var>) -> Term {
    match &t.kind {
        TermKind::Var(idx) => match substs[*idx] {
            Some(t) => renamed_copy(t, substs, new_substs),
            None => {
                new_substs.push(None);
                Term {
                    kind: TermKind::Var(new_substs.len() - 1),
                    location: t.location.clone(),
                }
            }
        },
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
    args: &[Term],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    let mut new_substs: Vec<Var> = Vec::new();
    let mut solutions: Vec<Term> = Vec::new();

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
