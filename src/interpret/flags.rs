use super::*;
use solve::{Continuation, Frame, Solver};
use term::*;

pub(super) fn solve_current_char_conversion(
    mut frame: Frame,
    a: usize,
    b: usize,
    next: &mut dyn Solver,
) -> Response {
    match frame.get_term(a) {
        Term::Atomic(t) => match &t.kind {
            read_term::TermKind::Atom(s1) if s1.len() == 1 => {
                match frame.get_term(b) {
                    Term::Atomic(b) => match &b.kind {
                        read_term::TermKind::Atom(s2) if s2.len() == 1 => {
                            if let Some(c) = frame
                                .get_context()
                                .char_conversion
                                .get(&s1.chars().next().unwrap())
                            {
                                if *c == s2.chars().next().unwrap() {
                                    return next.solve(frame);
                                }
                            }
                            Response::Fail
                        }
                        _ => todo!(), // type_error(callable(b))
                    },
                    Term::Var(idx) => {
                        if let Some(b) = frame.get_var(*idx) {
                            solve_current_char_conversion(frame, a, b, next)
                        } else {
                            if let Some(c) = frame
                                .get_context()
                                .char_conversion
                                .get(&s1.chars().next().unwrap())
                            {
                                let s = c.to_string();
                                frame.sub_frame(|mut frame| {
                                    let t = frame.new_term(&read_term::Term::new_atom(s, None));
                                    if frame.unify(t, b) {
                                        next.solve(frame)
                                    } else {
                                        Response::Fail
                                    }
                                })
                            } else {
                                Response::Fail
                            }
                        }
                    }
                    Term::Compound(c) => todo!(), // type_error(callable(c.compound))
                }
            }
            _ => todo!(), // type_error(callable(a))
        },
        Term::Var(idx) => {
            if let Some(a) = frame.get_var(*idx) {
                solve_current_char_conversion(frame, a, b, next)
            } else {
                match frame.get_term(b) {
                    Term::Atomic(t) => match &t.kind {
                        read_term::TermKind::Atom(s) if s.len() == 1 => {
                            // Reverse the map!
                            let c = s.chars().next().unwrap();
                            let mut m = Vec::new();
                            for (k, v) in &frame.get_context().char_conversion {
                                if *v == c {
                                    m.push(*k);
                                }
                            }

                            for c in m {
                                match frame.sub_frame(|mut frame| {
                                    let t = frame
                                        .new_term(&read_term::Term::new_atom(c.to_string(), None));
                                    if frame.unify(a, t) {
                                        next.solve(frame)
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
                        _ => todo!(), // type_error(callable(b))
                    },
                    Term::Var(idx) => {
                        if let Some(b) = frame.get_var(*idx) {
                            solve_current_char_conversion(frame, a, b, next)
                        } else {
                            // Clone the map
                            let m = frame.get_context().char_conversion.clone();
                            for (k, v) in m {
                                match frame.sub_frame(|mut frame| {
                                    let k = frame
                                        .new_term(&read_term::Term::new_atom(k.to_string(), None));
                                    if frame.unify(a, k) {
                                        let v = frame.new_term(&read_term::Term::new_atom(
                                            v.to_string(),
                                            None,
                                        ));
                                        if frame.unify(b, v) {
                                            next.solve(frame)
                                        } else {
                                            Response::Fail
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
                    }
                    Term::Compound(c) => todo!(), // type_error(callable(c.compound))
                }
            }
        }
        Term::Compound(c) => todo!(), // type_error(callable(c.compound))
    }
}
