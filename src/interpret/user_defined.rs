use super::*;
use solve::{Frame, Solver};
use term::*;

fn pi_term(term: &Rc<read_term::Term>) -> Rc<read_term::Term> {
    let (functor, arity) = match &term.kind {
        read_term::TermKind::Atom(s) => (
            read_term::Term::new_atom(s.clone(), None),
            read_term::Term::new_integer(0, None),
        ),
        read_term::TermKind::Compound(c) => (
            read_term::Term::new_atom(c.functor.clone(), None),
            read_term::Term::new_integer(c.args.len() as i64, None),
        ),
        _ => unreachable!(),
    };
    read_term::Term::new_compound("/".to_string(), None, vec![functor, arity])
}

fn existence_error(frame: &Frame, goal: usize) -> Response {
    // existence_error(procedure,pi)
    let pi = match frame.get_term(goal) {
        Term::Term(t) => pi_term(t),
        Term::Compound(c) => pi_term(&c.compound),
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

pub(super) fn solve(mut frame: Frame, pi: &str, goal: usize, next: &mut dyn Solver) -> Response {
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
            let head = frame.new_term_indexed(&clause.head, &mut index);
            if frame.unify(goal, head) {
                if let Some(body) = &clause.body {
                    let body = frame.new_term_indexed(body, &mut index);
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
                // type_error(callable,term)
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
                        read_term::TermKind::Var(idx) => {
                            // instantiation_error
                            todo!()
                        }
                        _ => {
                            // type_error(callable,c1.args[0])
                            todo!()
                        }
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
                // instantiation_error
                todo!()
            }
        }
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
                pi_term(term),
            ],
        ),
        frame.get_location().clone(),
    )
}

pub(super) fn assert(mut frame: Frame, goal: usize, is_z: bool, next: &mut dyn Solver) -> Response {
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
