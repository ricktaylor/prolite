use super::*;
use solve::{Continuation, Frame, Solver};
use term::*;

fn findall_var(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let mut solutions = Vec::new();
    frame
        .sub_frame(|frame| {
            solve::call(
                frame,
                args[1],
                &mut Continuation::new(|mut frame| {
                    solutions.push(frame.copy_term(args[0]));
                    Response::Fail
                }),
            )
        })
        .map_failed(|| {
            let list = frame.as_list(&solutions);
            solve::unify(frame, list, args[2], next)
        })
}

fn findall_list(
    mut frame: Frame,
    args: &[usize],
    mut head: usize,
    mut tail: usize,
    next: &mut dyn Solver,
) -> Response {
    let mut solutions = Vec::new();
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            args[1],
            &mut Continuation::new(|mut frame| {
                match frame.get_term(head) {
                    Term::Var(_) => {
                        solutions.push(frame.copy_term(args[0]));
                        return Response::Fail;
                    }
                    Term::Term(t) => {
                        if let read_term::TermKind::Atom(s) = &t.kind {
                            if s == "[]" {
                                // No more matches expected!
                                return Response::Cut;
                            }
                        }
                    }
                    _ => {}
                }

                if !frame.unify_copy(args[0], head) {
                    return Response::Cut;
                }

                match frame.get_term(tail) {
                    Term::Compound(c) if c.functor() == "." && c.args.len() == 2 => {
                        head = c.args[0];
                        tail = c.args[1];
                    }
                    _ => head = tail,
                }
                Response::Fail
            }),
        )
    }) {
        Response::Fail => {
            let list = frame.as_list(&solutions);
            solve::unify(frame, list, tail, next)
        }
        Response::Cut => Response::Fail,
        r => r,
    }
}

fn findall_none(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            args[1],
            &mut Continuation::new(|_| {
                // No solutions allowed
                Response::Cut
            }),
        )
    }) {
        Response::Fail => next.solve(frame),
        Response::Cut => Response::Fail,
        r => r,
    }
}

pub(super) fn solve_findall(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match frame.get_term(args[2]) {
        Term::Var(_) => return findall_var(frame, args, next),
        Term::Compound(c) if c.functor() == "." && c.args.len() == 2 => {
            let head = c.args[0];
            let tail = c.args[1];
            return findall_list(frame, args, head, tail, next);
        }
        Term::Term(t) => {
            if let read_term::TermKind::Atom(s) = &t.kind {
                if s == "[]" {
                    return findall_none(frame, args, next);
                }
            }
        }
        _ => {}
    }

    // Error
    todo!()
}
