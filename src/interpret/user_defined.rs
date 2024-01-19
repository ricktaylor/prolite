use super::*;
use solve::{Frame, Solver};
use term::*;

fn existence_error(frame: &Frame, goal: usize) -> Response {
    // existence_error(procedure,pi)
    let pi = match frame.get_term(goal) {
        Term::Atomic(t) => t.as_pi(),
        Term::Compound(c) => c.compound.as_pi(),
        _ => unreachable!(),
    };
    throw::error(
        read_term::Term::new_compound(
            "existence_error".to_string(),
            None,
            vec![read_term::Term::new_atom("procedure".to_string(), None), pi],
        ),
        frame.get_location().clone(),
    )
}

pub fn solve(mut frame: Frame, pi: &str, goal: usize, next: &mut dyn Solver) -> Response {
    let predicates = match frame.get_context().procedures.get(pi) {
        None => {
            match frame.get_context().flags.unknown {
                crate::flags::UnknownFlag::Error => return existence_error(&frame, goal),
                crate::flags::UnknownFlag::Warning => todo!(), // Some kind of warning?
                _ => {}
            }
            return Response::Fail;
        }
        Some(p) => p.predicates.to_vec(),
    };
    for clause in predicates {
        match frame.sub_frame(|mut frame| {
            let mut index = HashMap::new();

            // TODO:  We could merge new_term_indexed and unify here to one walk of the term tree
            let head = frame.new_term_indexed(clause.head.clone(), &mut index);
            if frame.unify(goal, head) {
                if let Some(body) = &clause.body {
                    let body = frame.new_term_indexed(body.clone(), &mut index);
                    solve::solve(frame, body, next)
                } else {
                    next.solve(frame)
                }
            } else {
                Response::Fail
            }
        }) {
            Response::Fail => {}
            r => return r,
        }
    }
    Response::Fail
}

fn unpack_pi(frame: &Frame, term: usize) -> Result<(String, Clause), Response> {
    match frame.get_term(term) {
        Term::Atomic(t) => {
            if let read_term::TermKind::Atom(s) = &t.kind {
                Ok((
                    format!("{}/0", s),
                    Clause {
                        head: t.clone(),
                        body: None,
                    },
                ))
            } else {
                // type_error(callable,term)
                todo!()
            }
        }
        Term::Compound(c) => {
            let c1 = c.as_compound();
            if c1.functor == ":-" && c1.args.len() == 2 {
                Ok((
                    match frame.get_term(c.args[0]) {
                        Term::Atomic(t) => {
                            if let read_term::TermKind::Atom(s) = &t.kind {
                                format!("{}/0", s)
                            } else {
                                // type_error(callable,term)
                                todo!()
                            }
                        }
                        Term::Var(_) => {
                            return Err(throw::instantiation_error(frame));
                        }
                        Term::Compound(c) => format!("{}/{}", c.functor(), c.args.len()),
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
        Term::Var(_) => Err(throw::instantiation_error(frame)),
    }
}

fn permission_error(frame: Frame, term: &Rc<read_term::Term>) -> Response {
    // permission_error(modify,static_procedure,pi)
    throw::error(
        read_term::Term::new_compound(
            "permission_error".to_string(),
            None,
            vec![
                read_term::Term::new_atom("modify".to_string(), None),
                read_term::Term::new_atom("static_procedure".to_string(), None),
                term.as_pi(),
            ],
        ),
        frame.get_location().clone(),
    )
}

pub fn assert(mut frame: Frame, goal: usize, is_z: bool, next: &mut dyn Solver) -> Response {
    unpack_pi(&frame, goal).map_or_else(
        |r| r,
        |(pi, clause)| {
            if builtins::get_builtin(&pi).is_some() || pi.starts_with("call/") {
                return permission_error(frame, &clause.head);
            }

            let procedures = &mut frame.get_context_mut().procedures;
            if let Some(p) = procedures.get_mut(&pi) {
                if !p.flags.dynamic {
                    return permission_error(frame, &clause.head);
                }

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
