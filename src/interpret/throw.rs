use super::*;
use frame::Frame;
use solve::{Continuation, Solver};
use term::TermKind;

pub fn instantiation_error(culprit: &Rc<read_term::Term>) -> Response {
    error(
        read_term::Term::new_atom("instantiation_error".to_string(), None),
        culprit.location.clone(),
    )
}

pub fn type_error(expected: &str, culprit: &Rc<read_term::Term>) -> Response {
    error(
        read_term::Term::new_compound(
            "type_error".to_string(),
            None,
            vec![
                read_term::Term::new_atom(expected.to_string(), None),
                culprit.clone(),
            ],
        ),
        culprit.location.clone(),
    )
}

pub fn callable(culprit: &Rc<read_term::Term>) -> Response {
    error(
        read_term::Term::new_compound(
            "existence_error".to_string(),
            None,
            vec![
                read_term::Term::new_atom("procedure".to_string(), None),
                culprit.as_pi().unwrap(),
            ],
        ),
        culprit.location.clone(),
    )
}

pub fn error(culprit: Rc<read_term::Term>, location: Option<stream::Span>) -> Response {
    let mut args = vec![culprit];
    if let Some(location) = &location {
        args.push(read_term::Term::new_atom(
            format!(
                "{}:{}:{}",
                location.start.source, location.start.line, location.start.column
            ),
            None,
        ))
    } else {
        args.push(read_term::Term::new_atom("unknown".to_string(), None));
    }
    Response::Throw(read_term::Term::new_compound(
        "error".to_string(),
        None,
        args,
    ))
}

fn var_check(frame: &Frame, ball: usize) -> Result<Rc<read_term::Term>, Response> {
    let (ball, _) = frame.get_term(ball);
    match &ball.kind {
        TermKind::Atomic => Ok(ball.source.clone()),
        TermKind::Var(_) => Err(throw::instantiation_error(&ball.source)),
        TermKind::Compound(args) => {
            for a in args.iter() {
                var_check(frame, *a)?;
            }
            Ok(ball.source.clone())
        }
    }
}

pub fn solve_throw(frame: Frame, args: &[usize], _: &mut dyn Solver) -> Response {
    match var_check(&frame, args[0]) {
        Err(r) => r,
        Ok(ball) => Response::Throw(ball),
    }
}

pub fn solve_catch(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let mut next_throw = false;
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            args[0],
            &mut Continuation::new(|frame| match next.solve(frame) {
                Response::Throw(t) => {
                    next_throw = true;
                    Response::Throw(t)
                }
                r => r,
            }),
        )
    }) {
        Response::Throw(ball) if !next_throw => frame.sub_frame(|mut frame| {
            let a = frame.new_term(&ball);
            if frame.unify(a, args[1]) {
                solve::call(frame, args[2], next)
            } else {
                Response::Throw(ball)
            }
        }),
        r => r,
    }
}
