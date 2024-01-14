use super::*;
use solve::{Frame, Solver};

fn write_list(frame: &Frame, head: usize, tail: usize) -> String {
    match frame.get_term(tail) {
        term::Term::Term(t) => match &t.kind {
            read_term::TermKind::Atom(a) if a == "[]" => write_term(frame, head),
            _ => panic!("malformed list!"),
        },
        term::Term::Var(idx) => match frame.get_var(*idx) {
            Some(t) => write_list(frame, head, t),
            None => format!("{}|_{}", write_term(frame, head), *idx),
        },
        term::Term::Compound(c) => match c.functor().as_str() {
            "." => format!(
                "{},{}",
                write_term(frame, head),
                write_list(frame, c.args[0], c.args[1])
            ),
            _ => panic!("malformed list!"),
        },
    }
}

pub(super) fn write_term(frame: &Frame, goal: usize) -> String {
    match frame.get_term(goal) {
        term::Term::Term(t) => match &t.kind {
            read_term::TermKind::Integer(i) => format!("{}", i),
            read_term::TermKind::Float(f) => format!("{}", f),
            read_term::TermKind::Atom(s) => s.clone(),
            _ => panic!("Mismatched term types!"),
        },
        term::Term::Var(idx) => {
            if let Some(t) = frame.get_var(*idx) {
                write_term(frame, t)
            } else {
                format!("_{}", *idx)
            }
        }
        term::Term::Compound(c) => match c.functor().as_str() {
            "." => {
                format!("[{}]", write_list(frame, c.args[0], c.args[1]))
            }
            s => {
                let mut s = s.to_string() + "(";
                for (i, a) in c.args.iter().enumerate() {
                    if i != 0 {
                        s += ",";
                    }
                    s += &write_term(frame, *a);
                }
                s + ")"
            }
        },
    }
}

pub(super) fn solve_write1(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    print!("{}", write_term(&frame, args[0]));
    next.solve(frame)
}

pub(super) fn solve_nl(frame: Frame, _: &[usize], next: &mut dyn Solver) -> Response {
    println!();
    next.solve(frame)
}
