use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

fn write_list(frame: &Frame, head: usize, tail: usize) -> String {
    let (tail, _) = frame.get_term(tail);
    match (&tail.kind, &tail.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Atom(a)) if a == "[]" => write_term(frame, head),
        (TermKind::Atomic, _) => panic!("malformed list!"),
        (TermKind::Var(idx), read_term::TermKind::Var(_)) => {
            format!("{}|_{}", write_term(frame, head), *idx)
        }
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            if c.functor == "." && args.len() == 2 {
                format!(
                    "{},{}",
                    write_term(frame, head),
                    write_list(frame, args[0], args[1])
                )
            } else {
                panic!("malformed list!")
            }
        }
        _ => unreachable!(),
    }
}

pub fn write_term(frame: &Frame, term: usize) -> String {
    let (term, _) = frame.get_term(term);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Integer(i)) => format!("{}", i),
        (TermKind::Atomic, read_term::TermKind::Float(f)) => format!("{}", f),
        (TermKind::Atomic, read_term::TermKind::Atom(s)) => s.clone(),
        (TermKind::Var(idx), read_term::TermKind::Var(_)) => format!("_{}", *idx),
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            if c.functor == "." {
                format!("[{}]", write_list(frame, args[0], args[1]))
            } else {
                let mut s = format!("{}(", c.functor);
                for (i, a) in args.iter().enumerate() {
                    if i != 0 {
                        s += ",";
                    }
                    s += &write_term(frame, *a);
                }
                s + ")"
            }
        }
        _ => unreachable!(),
    }
}

pub fn solve_write1(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    print!("{}", write_term(&frame, args[0]));
    next.solve(frame)
}

pub fn solve_nl0(frame: Frame, _: &[usize], next: &mut dyn Solver) -> Response {
    println!();
    next.solve(frame)
}
