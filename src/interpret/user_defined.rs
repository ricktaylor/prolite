use super::*;
use solve::{Continuation, Frame, Solver};
use term::*;

fn unknown_predicate() -> Response {
    todo!()
}

pub(super) fn solve(mut frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    let predicates = match frame.get_term(goal) {
        Term::Term(t) => {
            if let read_term::TermKind::Atom(s) = &t.kind {
                match frame.context().procedures.get(&format!("{}/0", s)) {
                    None => return unknown_predicate(),
                    Some(p) => p.predicates.to_vec(),
                }
            } else {
                todo!()
            }
        }
        Term::Compound(c) => {
            match frame
                .context()
                .procedures
                .get(&format!("{}/{}", c.functor(), c.args.len()))
            {
                None => return unknown_predicate(),
                Some(p) => p.predicates.to_vec(),
            }
        }
        _ => todo!(),
    };

    match frame.get_term(goal) {
        Term::Term(_) => {
            for clause in predicates {
                match frame.sub_frame(|mut frame| {
                    if let Some(body) = &clause.body {
                        let goal = frame.new_term(body);
                        solve::solve(frame, goal, next)
                    } else {
                        next.solve(frame)
                    }
                }) {
                    Response::Fail => {}
                    Response::Cut => return Response::Fail,
                    r => return r,
                }
            }
        }
        Term::Compound(c) => {
            let args = c.args.to_vec();
            for clause in predicates {
                let head_args = match &clause.head.kind {
                    read_term::TermKind::Compound(c) => &c.args,
                    _ => panic!("PI arity mismatch!"),
                };

                match frame.sub_frame(|mut frame| {
                    args.iter()
                        .zip(head_args)
                        .try_fold((), |_, (a, b)| {
                            let b = frame.new_term(b);
                            frame.unify_fold(*a, b)
                        })
                        .map_or_else(
                            |r| r,
                            |_| {
                                if let Some(body) = &clause.body {
                                    let goal = frame.new_term(body);
                                    solve::solve(
                                        frame,
                                        goal,
                                        &mut Continuation::new(|frame| next.solve(frame)),
                                    )
                                } else {
                                    next.solve(frame)
                                }
                            },
                        )
                }) {
                    Response::Fail => {}
                    Response::Cut => return Response::Fail,
                    r => return r,
                }
            }
        }
        _ => {}
    }
    Response::Fail
}
