use super::*;
use frame::Frame;
use solve::{Continuation, Solver};
use term::TermKind;

fn solve_var(
    mut frame: Frame,
    template: usize,
    goal: usize,
    instances: usize,
    next: &mut dyn Solver,
) -> Response {
    let mut solutions = Vec::new();
    frame
        .sub_frame(|frame| {
            solve::call(
                frame,
                goal,
                &mut Continuation::new(|mut frame| {
                    solutions.push(frame.copy_term(template));
                    Response::Fail
                }),
            )
        })
        .map_failed(|| {
            frame.sub_frame(|mut frame| {
                let list = frame.list_from_slice(&solutions);
                if frame.unify(list, instances) {
                    next.solve(frame)
                } else {
                    Response::Fail
                }
            })
        })
}

fn solve_list(
    mut frame: Frame,
    template: usize,
    goal: usize,
    mut head: usize,
    mut tail: usize,
    next: &mut dyn Solver,
) -> Response {
    let mut solutions = Vec::new();
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            goal,
            &mut Continuation::new(|mut frame| {
                let (term, h2) = frame.get_term(head);
                head = h2;

                match (&term.kind, &term.source.kind) {
                    (TermKind::Var(_), _) => {
                        solutions.push(frame.copy_term(template));
                        return Response::Fail;
                    }
                    (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => {
                        // No more matches expected!
                        return Response::Cut;
                    }
                    _ => {}
                }

                if !frame.unify_copy(template, head) {
                    return Response::Cut;
                }

                let (term, t2) = frame.get_term(tail);
                match (&term.kind, &term.source.kind) {
                    (TermKind::Compound(args), read_term::TermKind::Compound(c))
                        if c.functor == "." && args.len() == 2 =>
                    {
                        head = args[0];
                        tail = args[1];
                    }
                    _ => {
                        tail = t2;
                        head = tail
                    }
                }
                Response::Fail
            }),
        )
    }) {
        Response::Fail => frame.sub_frame(|mut frame| {
            let list = frame.list_from_slice(&solutions);
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

fn solve_none(mut frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            goal,
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

pub fn solve(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (instances_term, instances) = frame.get_term(args[2]);
    match (&instances_term.kind, &instances_term.source.kind) {
        (TermKind::Var(_), _) => solve_var(frame, args[0], args[1], args[2], next),
        (TermKind::Compound(c_args), read_term::TermKind::Compound(c))
            if c.functor == "." && c_args.len() == 2 =>
        {
            let head = c_args[0];
            let tail = c_args[1];
            solve_list(frame, args[0], args[1], head, tail, next)
        }
        (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => {
            solve_none(frame, args[1], next)
        }
        _ => {
            // Error
            todo!()
        }
    }
}
