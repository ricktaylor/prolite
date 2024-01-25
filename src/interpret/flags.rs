use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

pub fn solve_current_char_conversion(
    mut frame: Frame,
    t1: usize,
    t2: usize,
    next: &mut dyn Solver,
) -> Response {
    let (term1, t1) = frame.get_term(t1);
    let (term2, t2) = frame.get_term(t2);
    match (
        &term1.kind,
        &term1.source.kind,
        &term2.kind,
        &term2.source.kind,
    ) {
        (
            TermKind::Atomic,
            read_term::TermKind::Atom(s1),
            TermKind::Atomic,
            read_term::TermKind::Atom(s2),
        ) if s1.len() == 1 && s2.len() == 1 => {
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
        (TermKind::Atomic, read_term::TermKind::Atom(s), TermKind::Var(_), _) if s.len() == 1 => {
            if let Some(c) = frame
                .get_context()
                .char_conversion
                .get(&s.chars().next().unwrap())
            {
                let s = c.to_string();
                frame.sub_frame(|mut frame| {
                    let t1 = frame.new_term(&read_term::Term::new_atom(s, None));
                    if frame.unify(t1, t2) {
                        next.solve(frame)
                    } else {
                        Response::Fail
                    }
                });
            }
            Response::Fail
        }
        (TermKind::Var(_), _, TermKind::Atomic, read_term::TermKind::Atom(s)) if s.len() == 1 => {
            // Invert the map lookup!
            let c = s.chars().next().unwrap();
            let mut m = Vec::new();
            for (k, v) in &frame.get_context().char_conversion {
                if *v == c {
                    m.push(*k);
                }
            }
            for c in m {
                match frame.sub_frame(|mut frame| {
                    let t2 = frame.new_term(&read_term::Term::new_atom(c.to_string(), None));
                    if frame.unify(t1, t2) {
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
        (TermKind::Var(_), _, TermKind::Var(_), _) => {
            // Clone the map
            let m = frame.get_context().char_conversion.clone();
            for (k, v) in m {
                match frame.sub_frame(|mut frame| {
                    let k = frame.new_term(&read_term::Term::new_atom(k.to_string(), None));
                    if frame.unify(t1, k) {
                        let v = frame.new_term(&read_term::Term::new_atom(v.to_string(), None));
                        if frame.unify(v, t2) {
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
        _ => todo!(), // error!
    }
}
