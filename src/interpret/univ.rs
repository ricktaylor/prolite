use super::*;
use solve::{Frame, Solver};
use term::*;

fn univ_nonvar(mut frame: Frame, terms: &[usize], list: usize, next: &mut dyn Solver) -> Response {
    let list2 = frame.list_from_slice(terms);
    if frame.unify(list2, list) {
        next.solve(frame)
    } else {
        Response::Fail
    }
}

fn unpack_list(frame: &Frame, tail: usize, terms: &mut Vec<usize>) -> Result<(), Response> {
    match frame.get_term(tail) {
        Term::Atomic(t) => match &t.kind {
            read_term::TermKind::Atom(s) if s == "[]" => Ok(()),
            _ => unreachable!(),
        },
        Term::Var(_) => Err(throw::instantiation_error(frame)),
        Term::Compound(c) if c.functor() == "." && c.args.len() == 2 => {
            terms.push(c.args[0]);
            unpack_list(frame, c.args[1], terms)
        }
        _ => unreachable!(),
    }
}

fn univ_var(mut frame: Frame, term: usize, list: usize, next: &mut dyn Solver) -> Response {
    match frame.get_term(list) {
        Term::Atomic(t) => match &t.kind {
            read_term::TermKind::Atom(s) if s == "[]" => {
                todo!() // domain_error(non_empty_list,T)
            }
            _ => {
                todo!() // type_error(list,List)
            }
        },
        Term::Var(_) => throw::instantiation_error(&frame),
        Term::Compound(c) => {
            if c.functor() != "." || c.args.len() != 2 {
                todo!() // type_error(list,List)
            } else {
                match frame.get_term(c.args[0]) {
                    Term::Atomic(t) => match &t.kind {
                        read_term::TermKind::Atom(_) => {
                            let mut args = vec![c.args[0]];
                            let tail = c.args[1];
                            frame.sub_frame(|mut frame| {
                                unpack_list(&frame, tail, &mut args).map_or_else(
                                    |r| r,
                                    |_| {
                                        let list = frame.term_from_slice(&args);
                                        if frame.unify(term, list) {
                                            next.solve(frame)
                                        } else {
                                            Response::Fail
                                        }
                                    },
                                )
                            })
                        }
                        _ => todo!(), // type_error(atom,c.args[0])
                    },
                    Term::Var(_) => throw::instantiation_error(&frame),
                    Term::Compound(_) => {
                        if let Term::Atomic(tail) = frame.get_term(c.args[1]) {
                            if let read_term::TermKind::Atom(s) = &tail.kind {
                                if s == "[]" {
                                    todo!() // type_error(atomic,c.args[0])
                                }
                            }
                        }
                        todo!() // type_error(atom,c.args[0]),
                    }
                }
            }
        }
    }
}

pub fn solve(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match frame.get_term(args[0]) {
        Term::Atomic(_) => frame.sub_frame(|frame| univ_nonvar(frame, &[args[0]], args[1], next)),
        Term::Compound(c) => {
            let t = read_term::Term::new_atom(c.functor().clone(), None);
            let mut terms = c.args.to_vec();
            frame.sub_frame(|mut frame| {
                let mut t2 = vec![frame.new_term(t)];
                t2.append(&mut terms);
                univ_nonvar(frame, &t2, args[1], next)
            })
        }
        Term::Var(_) => univ_var(frame, args[0], args[1], next),
    }
}
