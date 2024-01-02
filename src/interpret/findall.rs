use bit_set::BitSet;

use super::*;
use solve::{Continuation, Solver};
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
    let mut new_substs = substs.to_vec();
    let mut solutions = Vec::new();

    solve::call(
        ctx,
        &args[1],
        substs,
        &mut Continuation::new(|_, inner_substs| {
            // TODO: We can early out here if args[2] is a list or partial list

            solutions.push(renamed_copy(&args[0], inner_substs, &mut new_substs));
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

fn deref_var<'a>(mut t: &'a Rc<Term>, substs: &[Var<'a>]) -> &'a Rc<Term> {
    match &t.kind {
        TermKind::Var(idx) => {
            if let Some(t) = substs[*idx] {
                deref_var(t, substs)
            } else {
                t
            }
        }
        _ => t,
    }
}

fn variable_set(t: &Rc<Term>, substs: &[Var], free_vars: &mut BitSet) {
    match &t.kind {
        TermKind::Var(idx) => {
            if let Some(t) = substs[*idx] {
                variable_set(t, substs, free_vars);
            } else {
                free_vars.insert(*idx);
            }
        }
        TermKind::Compound(c) => {
            for t in c.args.iter() {
                variable_set(t, substs, free_vars);
            }
        }
        _ => {}
    }
}

fn existential_split<'a>(
    t: &'a Rc<Term>,
    substs: &'a [Var],
    free_vars: &mut BitSet,
) -> &'a Rc<Term> {
    match &t.kind {
        TermKind::Var(idx) => {
            if let Some(t) = substs[*idx] {
                existential_split(t, substs, free_vars)
            } else {
                t
            }
        }
        TermKind::Compound(c) if c.functor == "^" && c.args.len() == 2 => {
            variable_set(&c.args[0], substs, free_vars);
            existential_split(&c.args[1], substs, free_vars)
        }
        _ => t,
    }
}

fn split_free_vars<'a>(
    t: &'a Rc<Term>,
    v: &'a Rc<Term>,
    substs: &'a [Var],
) -> (&'a Rc<Term>, BitSet) {
    let mut free_vars = BitSet::with_capacity(substs.len());
    variable_set(t, substs, &mut free_vars);

    let mut other_vars = BitSet::with_capacity(substs.len());
    variable_set(v, substs, &mut other_vars);

    let goal = existential_split(t, substs, &mut other_vars);
    free_vars.difference_with(&other_vars);

    (goal, free_vars)
}

pub(super) fn solve_setof(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {

    // Find the free variables of the iterated goal of arg[1] wrt arg[0]
    let (goal, free_vars) = split_free_vars(&args[1], &args[0], substs);

    let mut new_substs = substs.to_vec();
    let mut solutions = Vec::new();
    solve::call(
        ctx,
        goal,
        substs,
        &mut Continuation::new(|_, inner_substs| {
            // Accumulate the current values of the free variables
            let free_values: Vec<Option<Rc<Term>>> = free_vars
                .iter()
                .map(|idx| {
                    inner_substs[idx].map(|t| {
                        // TODO -  We can inline deref into a loop
                        deref_var(t, inner_substs).clone()
                    })
                })
                .collect();

            let template = renamed_copy(&args[0], inner_substs, &mut new_substs);

            //MUCH MORE HERE!

            solutions.push((free_values, template));
            Response::Fail
        }),
    )
    .map_failed(|| {
        while !solutions.is_empty() {}

        Response::Fail
    })
}
