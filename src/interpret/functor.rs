use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

pub fn solve_functor(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (term, t) = frame.get_term(args[0]);
    let (name, n) = frame.get_term(args[1]);
    let (arity, a) = frame.get_term(args[2]);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, _) => frame.sub_frame(|mut frame| {
            if frame.unify(args[1], t) {
                let zero = frame.new_term(&read_term::Term::new_integer(0, None));
                if frame.unify(args[2], zero) {
                    return next.solve(frame);
                }
            }
            Response::Fail
        }),
        (TermKind::Var(_), _) => match (
            &name.kind,
            &name.source.kind,
            &arity.kind,
            &arity.source.kind,
        ) {
            (TermKind::Var(_), _, _, _) => throw::instantiation_error(&name.source),
            (_, _, TermKind::Var(_), _) => throw::instantiation_error(&arity.source),
            (TermKind::Atomic, _, TermKind::Atomic, read_term::TermKind::Integer(0)) => frame
                .sub_frame(|mut frame| {
                    if frame.unify(t, n) {
                        next.solve(frame)
                    } else {
                        Response::Fail
                    }
                }),
            (
                TermKind::Atomic,
                read_term::TermKind::Atom(s),
                TermKind::Atomic,
                read_term::TermKind::Integer(i),
            ) if *i > 0 => {
                if (*i as u64) > (usize::MAX as u64) {
                    throw::error(
                        read_term::Term::new_compound(
                            "representation_error".to_string(),
                            None,
                            vec![read_term::Term::new_atom("max_arity".to_string(), None)],
                        ),
                        arity.source.location.clone(),
                    )
                } else {
                    let mut args = Vec::new();
                    for i in 0..*i {
                        args.push(Rc::new(read_term::Term {
                            kind: read_term::TermKind::Var(i as usize),
                            location: None,
                        }));
                    }
                    let c = read_term::Term::new_compound(s.clone(), None, args);
                    frame.sub_frame(|mut frame| {
                        let c = frame.new_term(&c);
                        if frame.unify(t, c) {
                            next.solve(frame)
                        } else {
                            Response::Fail
                        }
                    })
                }
            }
            (
                TermKind::Atomic,
                read_term::TermKind::Atom(_),
                TermKind::Atomic,
                read_term::TermKind::Integer(_),
            ) => throw::error(
                read_term::Term::new_compound(
                    "domain_error".to_string(),
                    None,
                    vec![
                        read_term::Term::new_atom("not_less_than_zero".to_string(), None),
                        arity.source.clone(),
                    ],
                ),
                arity.source.location.clone(),
            ),
            (TermKind::Atomic, _, TermKind::Atomic, read_term::TermKind::Integer(_)) => {
                throw::type_error("atom", &name.source)
            }
            (TermKind::Atomic, _, _, _) => throw::type_error("integer", &arity.source),
            _ => throw::type_error("atomic", &name.source),
        },
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            let functor = read_term::Term::new_atom(c.functor.clone(), None);
            let arity = read_term::Term::new_integer(args.len() as i64, None);
            frame.sub_frame(|mut frame| {
                let functor = frame.new_term(&functor);
                let arity = frame.new_term(&arity);
                if frame.unify(n, functor) && frame.unify(a, arity) {
                    next.solve(frame)
                } else {
                    Response::Fail
                }
            })
        }
        _ => unreachable!(),
    }
}

pub fn solve_arg(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (n, _) = frame.get_term(args[0]);
    let (term, _) = frame.get_term(args[1]);
    match (&n.kind, &n.source.kind, &term.kind) {
        (TermKind::Var(_), _, _) => throw::instantiation_error(&n.source),
        (_, _, TermKind::Var(_)) => throw::instantiation_error(&term.source),
        (TermKind::Atomic, read_term::TermKind::Integer(i), TermKind::Compound(c_args)) => {
            if *i < 0 {
                throw::error(
                    read_term::Term::new_compound(
                        "domain_error".to_string(),
                        None,
                        vec![
                            read_term::Term::new_atom("not_less_than_zero".to_string(), None),
                            n.source.clone(),
                        ],
                    ),
                    n.source.location.clone(),
                )
            } else if *i > 0 && (*i as u64) <= (c_args.len() as u64) {
                let t1 = c_args[(*i - 1) as usize];
                frame.sub_frame(|mut frame| {
                    if frame.unify(t1, args[2]) {
                        next.solve(frame)
                    } else {
                        Response::Fail
                    }
                })
            } else {
                Response::Fail
            }
        }
        (TermKind::Atomic, read_term::TermKind::Integer(_), _) => {
            throw::type_error("compound", &term.source)
        }
        _ => throw::type_error("integer", &n.source),
    }
}
