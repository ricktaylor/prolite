use std::rc::Rc;

use super::*;
use term::*;

fn unknown_predicate(ctx: &mut Context) -> Response {
    todo!()
}

pub(super) fn solve(
    ctx: &mut Context,
    goal: &Term,
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    match &goal.kind {
        TermKind::Atom(s) => {
            let pi = format!("{}/0", s);

            println!(
                "User defined function: {} at {}:{}:{}",
                pi,
                goal.location.start.source,
                goal.location.start.line,
                goal.location.start.column
            );

            match ctx.procedures.get(&pi) {
                None => unknown_predicate(ctx),
                Some(p) => {
                    let predicates = p.predicates.to_vec();
                    for clause in predicates {
                        match solve::solve(
                            ctx,
                            &clause.body,
                            &vec![None; clause.var_info.len()],
                            next,
                        ) {
                            Response::Fail => {}
                            Response::Cut => return Response::Fail,
                            r => return r,
                        }
                    }
                    Response::Fail
                }
            }
        }
        TermKind::Compound(c) => {
            let pi = format!("{}/{}", c.functor, c.args.len());

            println!(
                "User defined function: {} at {}:{}:{}",
                pi,
                goal.location.start.source,
                goal.location.start.line,
                goal.location.start.column
            );

            match ctx.procedures.get(&pi) {
                None => unknown_predicate(ctx),
                Some(p) => {
                    let predicates = p.predicates.to_vec();
                    for clause in predicates {
                        match solve_clause(ctx, &c.args, clause, substs, next) {
                            Response::Fail => {}
                            Response::Cut => return Response::Fail,
                            r => return r,
                        }
                    }
                    Response::Fail
                }
            }
        }
        _ => todo!(),
    }
}

struct Substitutions<'a> {
    a: Vec<Var<'a>>,
    b: Vec<Var<'a>>,
    out: HashMap<usize, &'a Term>,
}

impl<'a> Substitutions<'a> {
    fn new(a: &'a [Var<'a>], b_len: usize) -> Self {
        Self {
            a: a.to_vec(),
            b: vec![None; b_len],
            out: HashMap::new(),
        }
    }
}

fn unify_head<'a>(
    a: &'a Term,
    b: &'a Term,
    mut substs: Substitutions<'a>,
) -> Result<Substitutions<'a>, Response> {
    match &a.kind {
        TermKind::Var(idx1) => {
            if let Some(a) = substs.a[*idx1] {
                unify_head(a, b, substs)
            } else {
                match &b.kind {
                    TermKind::Var(idx2) => {
                        if let Some(b) = substs.b[*idx2] {
                            unify_head(a, b, substs)
                        } else {
                            if let Some(&t) = substs.out.get(idx1) {
                                substs.b[*idx2] = Some(t);
                            } else {
                                substs.out.insert(*idx1, b);
                            }
                            Ok(substs)
                        }
                    }
                    _ => {
                        substs.a[*idx1] = Some(b);
                        Ok(substs)
                    }
                }
            }
        }
        TermKind::Integer(i1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_head(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Integer(i2) if *i1 == *i2 => Ok(substs),
            TermKind::Float(f) if *i1 as f64 == *f => Ok(substs),
            _ => Err(Response::Fail),
        },
        TermKind::Float(f1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_head(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Float(f2) if *f1 == *f2 => Ok(substs),
            TermKind::Integer(i) if *f1 == *i as f64 => Ok(substs),
            _ => Err(Response::Fail),
        },
        TermKind::Atom(s1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_head(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Atom(s2) if *s1 == *s2 => Ok(substs),
            _ => Err(Response::Fail),
        },
        TermKind::Compound(c1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_head(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Compound(c2)
                if c1.functor == c2.functor && c1.args.len() == c2.args.len() =>
            {
                c1.args
                    .iter()
                    .zip(&c2.args)
                    .try_fold(substs, |substs, (a, b)| unify_head(a, b, substs))
            }
            _ => Err(Response::Fail),
        },
    }
}

fn solve_clause(
    ctx: &mut Context,
    args: &[Term],
    clause: Rc<Clause>,
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    let head = match &clause.head.kind {
        TermKind::Compound(c) => c,
        _ => panic!("PI arity mismatch!"),
    };

    // Unify head args
    args.iter()
        .zip(&head.args)
        .try_fold(
            Substitutions::new(substs, clause.var_info.len()),
            |s, (a, b)| unify_head(a, b, s),
        )
        .map_or_else(
            |r| r,
            |mut s| {
                solve::solve(
                    ctx,
                    &clause.body,
                    &s.b,
                    &mut Continuation::new(|ctx, b_substs| {
                        next.solve(
                            ctx,
                            &s.out.iter().fold(
                                std::mem::take(&mut s.a),
                                |mut a_substs, (idx, &t)| {
                                    a_substs[*idx] = Some(deref_var(t, b_substs));
                                    a_substs
                                },
                            ),
                        )
                    }),
                )
            },
        )
}
