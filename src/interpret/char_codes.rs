use super::term::TermKind;
use super::*;
use frame::Frame;
use solve::Solver;

fn parse_term(frame: &Frame, s: &str) -> super::super::read_term::Response {
    super::super::read_term::Context {
        flags: &frame.context.flags,
        operators: &frame.context.operators,
        char_conversion: &frame.context.char_conversion,
        greedy: false,
    }
    .read_term(s)
}

fn chars_to_string(frame: &Frame, mut list: usize) -> Result<String, Response> {
    let mut result = String::new();
    loop {
        let (term, _) = frame.get_term(list);
        match (&term.kind, &term.source.kind) {
            (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => break,
            (TermKind::Compound(args), read_term::TermKind::Compound(c))
                if c.functor == "." && args.len() == 2 =>
            {
                let (head, _) = frame.get_term(args[0]);
                match (&head.kind, &head.source.kind) {
                    (TermKind::Atomic, read_term::TermKind::Atom(s)) if s.len() == 1 => {
                        result += s;
                        list = args[1];
                    }
                    (TermKind::Var(_), _) => return Err(throw::instantiation_error(&head.source)),
                    _ => {
                        return Err(throw::type_error("character", &head.source));
                    }
                }
            }
            (TermKind::Var(_), _) => return Err(throw::instantiation_error(&term.source)),
            _ => {
                return Err(throw::type_error("list", &term.source));
            }
        }
    }
    Ok(result)
}

pub fn solve_number_chars(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (term1, t1) = frame.get_term(args[0]);
    match (&term1.kind, &term1.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Integer(_))
        | (TermKind::Atomic, read_term::TermKind::Float(_)) => {
            let (term2, t2) = frame.get_term(args[1]);
            // write_term(t1)
            // unify with t2
            todo!()
        }
        (TermKind::Var(_), _) => {
            let (term2, t2) = frame.get_term(args[1]);
            match chars_to_string(&frame, t2) {
                Ok(s) => match parse_term(&frame, &s) {
                    Err(_) | Ok(None) => throw::error(
                        read_term::Term::new_compound(
                            "syntax_error".to_string(),
                            None,
                            vec![read_term::Term::new_atom("not_a_number".to_string(), None)],
                        ),
                        term2.source.location.clone(),
                    ),
                    Ok(Some((term2, _))) => match &term2.kind {
                        read_term::TermKind::Integer(_) | read_term::TermKind::Float(_) => frame
                            .sub_frame(|mut frame| {
                                let t2 = frame.new_term(&term2);
                                if frame.unify(t1, t2) {
                                    next.solve(frame)
                                } else {
                                    Response::Fail
                                }
                            }),
                        _ => throw::error(
                            read_term::Term::new_compound(
                                "syntax_error".to_string(),
                                None,
                                vec![read_term::Term::new_atom("not_a_number".to_string(), None)],
                            ),
                            term2.location.clone(),
                        ),
                    },
                },
                Err(r) => r,
            }
        }
        _ => throw::type_error("number", &term1.source),
    }
}
