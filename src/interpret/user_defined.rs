use super::*;
use solve::{Frame, Solver};
use term::*;

fn unknown_predicate(_frame: Frame) -> Response {
    todo!()
}

fn unpack_pi(frame: &Frame, term: usize) -> Result<(String, Clause), Response> {
    match frame.get_term(term) {
        Term::Term(t) => {
            if let read_term::TermKind::Atom(s) = &t.kind {
                Ok((
                    format!("{}/0", s),
                    Clause {
                        head: t.clone(),
                        body: None,
                    },
                ))
            } else {
                todo!()
            }
        }
        Term::Compound(c) => {
            let c1 = c.as_compound();
            if c1.functor == ":-" && c1.args.len() == 2 {
                Ok((
                    match &c1.args[0].kind {
                        read_term::TermKind::Atom(s) => format!("{}/0", *s),
                        read_term::TermKind::Compound(c) => {
                            format!("{}/{}", c.functor, c.args.len())
                        }
                        _ => todo!(),
                    },
                    Clause {
                        head: c1.args[0].clone(),
                        body: Some(c1.args[1].clone()),
                    },
                ))
            } else {
                Ok((
                    format!("{}/{}", c1.functor, c1.args.len()),
                    Clause {
                        head: c.compound.clone(),
                        body: None,
                    },
                ))
            }
        }
        Term::Var(idx) => {
            if let Some(goal) = frame.get_var(*idx) {
                unpack_pi(frame, goal)
            } else {
                todo!()
            }
        }
    }
}

pub(super) fn assert(mut frame: Frame, goal: usize, is_z: bool, next: &mut dyn Solver) -> Response {
    unpack_pi(&frame, goal).map_or_else(
        |r| r,
        |(pi, clause)| {
            if builtins::is_builtin(&pi).is_some() || pi.starts_with("call/") {
                todo!()
            }

            let procedures = &mut frame.get_context_mut().procedures;
            if let Some(p) = procedures.get_mut(&pi) {
                if is_z {
                    p.predicates.push(Rc::new(clause));
                } else {
                    p.predicates.insert(0, Rc::new(clause));
                }
            } else {
                procedures.insert(
                    pi.clone(),
                    Procedure {
                        predicates: vec![Rc::new(clause)],
                        ..Procedure::default()
                    },
                );
            }
            next.solve(frame)
        },
    )
}

pub(super) fn solve(mut frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    match frame.get_term(goal) {
        Term::Term(t) => {
            let predicates = if let read_term::TermKind::Atom(s) = &t.kind {
                match frame.get_context().procedures.get(&format!("{}/0", s)) {
                    None => return unknown_predicate(frame),
                    Some(p) => p.predicates.to_vec(),
                }
            } else {
                todo!()
            };

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
            let predicates = match frame.get_context().procedures.get(&format!(
                "{}/{}",
                c.functor(),
                c.args.len()
            )) {
                None => return unknown_predicate(frame),
                Some(p) => p.predicates.to_vec(),
            };

            for clause in predicates {
                match frame.sub_frame(|mut frame| {

                    let mut index =  HashMap::new();
                    let head = frame.new_term_indexed(&clause.head, &mut index);

                    if frame.unify(goal, head) {
                        if let Some(body) = &clause.body {
                            let body = frame.new_term_indexed(body, &mut index);
                            solve::solve(frame,body,next)
                        } else {
                            next.solve(frame)
                        }
                    } else {
                        Response::Fail
                    }
                }){
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
