use super::*;
use solve::{Frame, Solver};
use term::*;

pub(super) fn error(culprit: Rc<read_term::Term>, location: Option<stream::Span>) -> Response {
    let mut args = vec![culprit];
    if let Some(location) = &location {
        args.push(read_term::Term::new_atom(
            format!(
                "{}:{}:{}",
                location.start.source, location.start.line, location.start.column
            ),
            None,
        ))
    }
    Response::Throw(read_term::Term::new_compound(
        "error".to_string(),
        None,
        args,
    ))
}

fn var_check(frame: &Frame, ball: usize) -> Result<Rc<read_term::Term>, Response> {
    match frame.get_term(ball) {
        Term::Term(t) => Ok(t.clone()),
        Term::Var(v) => {
            // instantiation_error
            todo!()
        }
        Term::Compound(c) => {
            for a in c.args.iter() {
                var_check(frame, *a)?;
            }
            Ok(c.compound.clone())
        }
    }
}

pub(super) fn solve(frame: Frame, args: &[usize], _: &mut dyn Solver) -> Response {
    match var_check(&frame, args[0]) {
        Err(r) => r,
        Ok(ball) => Response::Throw(ball),
    }
}
