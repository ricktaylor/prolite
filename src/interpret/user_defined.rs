use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

fn unpack_pi(frame: &Frame, term: usize) -> Result<(String, Clause), Response> {
    let (term, _) = frame.get_term(term);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Atom(s)) => Ok((
            format!("{}/0", s),
            Clause {
                head: term.source.clone(),
                body: None,
            },
        )),
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            if c.functor == ":-" && args.len() == 2 {
                let (head, _) = frame.get_term(args[0]);
                let functor = match (&head.kind, &head.source.kind) {
                    (TermKind::Atomic, read_term::TermKind::Atom(s)) => format!("{}/0", s),
                    (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
                        format!("{}/{}", c.functor, args.len())
                    }
                    (TermKind::Var(_), _) => return Err(throw::instantiation_error(&head.source)),
                    _ => return Err(throw::callable(&head.source)),
                };
                let (body, _) = frame.get_term(args[1]);
                Ok((
                    functor,
                    Clause {
                        head: head.source.clone(),
                        body: Some(body.source.clone()),
                    },
                ))
            } else {
                Ok((
                    format!("{}/{}", c.functor, args.len()),
                    Clause {
                        head: term.source.clone(),
                        body: None,
                    },
                ))
            }
        }
        (TermKind::Var(_), _) => Err(throw::instantiation_error(&term.source)),
        _ => Err(throw::callable(&term.source)),
    }
}

fn permission_error(culprit: &Rc<read_term::Term>) -> Response {
    throw::error(
        read_term::Term::new_compound(
            "permission_error".to_string(),
            None,
            vec![
                read_term::Term::new_atom("modify".to_string(), None),
                read_term::Term::new_atom("static_procedure".to_string(), None),
                culprit.as_pi().unwrap(),
            ],
        ),
        culprit.location.clone(),
    )
}

pub fn assert(frame: Frame, goal: usize, is_z: bool, next: &mut dyn Solver) -> Response {
    unpack_pi(&frame, goal).map_or_else(
        |r| r,
        |(pi, clause)| {
            if builtins::get_builtin(&pi).is_some() {
                return permission_error(&clause.head);
            }

            let procedures = &mut frame.context.procedures;
            if let Some(p) = procedures.get_mut(&pi) {
                if !p.flags.dynamic {
                    return permission_error(&clause.head);
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
