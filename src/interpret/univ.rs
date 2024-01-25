use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

fn univ_nonvar(mut frame: Frame, terms: &[usize], list: usize, next: &mut dyn Solver) -> Response {
    let list2 = frame.list_from_slice(terms);
    if frame.unify(list2, list) {
        next.solve(frame)
    } else {
        Response::Fail
    }
}

fn unpack_list(frame: &Frame, tail: usize, terms: &mut Vec<usize>) -> Result<bool, Response> {
    let (term, tail) = frame.get_term(tail);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => Ok(true),
        (TermKind::Var(_), _) => Err(throw::instantiation_error(&term.source)),
        (TermKind::Compound(args), read_term::TermKind::Compound(c))
            if c.functor == "." && args.len() == 2 =>
        {
            terms.push(args[0]);
            unpack_list(frame, args[1], terms)
        }
        _ => Ok(false),
    }
}

fn univ_var(mut frame: Frame, term: usize, list: usize, next: &mut dyn Solver) -> Response {
    let (list_term, list) = frame.get_term(list);
    match (&list_term.kind, &list_term.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => {
            todo!() // domain_error(non_empty_list,T)
        }
        (TermKind::Var(_), _) => throw::instantiation_error(&list_term.source),
        (TermKind::Compound(c_args), read_term::TermKind::Compound(c))
            if c.functor == "." && c_args.len() == 2 =>
        {
            let (head, h) = frame.get_term(c_args[0]);
            match (&head.kind, &head.source.kind) {
                (TermKind::Atomic, read_term::TermKind::Atom(_)) => {
                    let mut args = vec![h];
                    unpack_list(&frame, c_args[1], &mut args).map_or_else(
                        |r| r,
                        |is_list| {
                            if !is_list {
                                todo!() // type_error(list,List)
                            } else {
                                frame.sub_frame(|mut frame| {
                                    let list = frame.term_from_slice(&args).unwrap();
                                    if frame.unify(term, list) {
                                        next.solve(frame)
                                    } else {
                                        Response::Fail
                                    }
                                })
                            }
                        },
                    )
                }
                (TermKind::Var(_), _) => throw::instantiation_error(&head.source),
                (TermKind::Compound(_), _) => {
                    let (tail, _) = frame.get_term(c_args[1]);
                    match (&tail.kind, &tail.source.kind) {
                        (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => {
                            todo!() // type_error(atomic,h)
                        }
                        _ => todo!(), // type_error(atom,h),
                    }
                }
                _ => todo!(), // type_error(atom,h),
            }
        }
        _ => {
            todo!() // type_error(list,List)
        }
    }
}

pub fn solve(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (term, t) = frame.get_term(args[0]);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, _) => frame.sub_frame(|frame| univ_nonvar(frame, &[t], args[1], next)),
        (TermKind::Compound(c_args), read_term::TermKind::Compound(c)) => {
            let t = read_term::Term::new_atom(c.functor.clone(), None);
            let mut terms = c_args.to_vec();
            frame.sub_frame(|mut frame| {
                let mut t2 = vec![frame.new_term(&t)];
                t2.append(&mut terms);
                univ_nonvar(frame, &t2, args[1], next)
            })
        }
        (TermKind::Var(_), _) => univ_var(frame, t, args[1], next),
        _ => unreachable!(),
    }
}
