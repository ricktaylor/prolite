use super::*;
use solve::{Continuation, Frame, Solver};
use term::*;

fn solve_var(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
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
            frame.sub_frame(|mut frame| {
                let list = frame.as_list(&solutions);
                if frame.unify(list, args[2]) {
                    next.solve(frame)
                } else {
                    Response::Fail
                }
            })
        })
}

fn solve_list(
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
                    Term::Atomic(t) => {
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
        Response::Fail => frame.sub_frame(|mut frame| {
            let list = frame.as_list(&solutions);
            if frame.unify(list, tail) {
                next.solve(frame)
            } else {
                Response::Fail
            }
        }),
        Response::Cut => Response::Fail,
        r => r,
    }
}

fn solve_none(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
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

pub(super) fn solve(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match frame.get_term(args[2]) {
        Term::Var(_) => return solve_var(frame, args, next),
        Term::Compound(c) if c.functor() == "." && c.args.len() == 2 => {
            let head = c.args[0];
            let tail = c.args[1];
            return solve_list(frame, args, head, tail, next);
        }
        Term::Atomic(t) => {
            if let read_term::TermKind::Atom(s) = &t.kind {
                if s == "[]" {
                    return solve_none(frame, args, next);
                }
            }
        }
        _ => {}
    }

    // Error
    todo!()
}
