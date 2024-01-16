use phf::phf_map;

use super::*;
use solve::{Frame, Solver};
use term::*;

fn throw_evaluable(term: &Rc<read_term::Term>) -> Response {
    throw::error(
        read_term::Term::new_compound("evaluable".to_string(), None, vec![term.as_pi()]),
        term.location.clone(),
    )
}

fn compare_with_overflow(frame: &Frame, i: &i64) -> Result<f64, Response> {
    let f = *i as f64;
    if *i != f as i64 {
        Err(throw::error(
            read_term::Term::new_compound(
                "evaluation_error".to_string(),
                None,
                vec![read_term::Term::new_atom(
                    "float_overflow".to_string(),
                    None,
                )],
            ),
            frame.get_location().clone(),
        ))
    } else {
        Ok(f)
    }
}

fn compare(
    frame: &mut Frame,
    t1: usize,
    t2: usize,
) -> Result<Option<core::cmp::Ordering>, Response> {
    match (frame.get_term(t1), frame.get_term(t2)) {
        (Term::Var(idx), _) => {
            if let Some(t1) = frame.get_var(*idx) {
                compare(frame, t1, t2)
            } else {
                Err(throw::instantiation_error(frame))
            }
        }
        (_, Term::Var(idx)) => {
            if let Some(t2) = frame.get_var(*idx) {
                compare(frame, t1, t2)
            } else {
                Err(throw::instantiation_error(frame))
            }
        }
        (Term::Term(t1), Term::Term(t2)) => match (&t1.kind, &t2.kind) {
            (read_term::TermKind::Atom(_), _) => Err(throw_evaluable(t1)),
            (_, read_term::TermKind::Atom(_)) => Err(throw_evaluable(t2)),
            (read_term::TermKind::Integer(i1), read_term::TermKind::Integer(i2)) => {
                Ok(core::cmp::PartialOrd::partial_cmp(i1, i2))
            }
            (read_term::TermKind::Integer(i1), read_term::TermKind::Float(d2)) => Ok(
                core::cmp::PartialOrd::partial_cmp(&compare_with_overflow(frame, i1)?, d2),
            ),
            (read_term::TermKind::Float(d1), read_term::TermKind::Integer(i2)) => Ok(
                core::cmp::PartialOrd::partial_cmp(d1, &compare_with_overflow(frame, i2)?),
            ),
            (read_term::TermKind::Float(d1), read_term::TermKind::Float(d2)) => {
                Ok(core::cmp::PartialOrd::partial_cmp(d1, d2))
            }
            _ => unreachable!(),
        },
        (Term::Compound(c), _) => Err(throw_evaluable(&c.compound)),
        (_, Term::Compound(c)) => Err(throw_evaluable(&c.compound)),
    }
}

pub(super) fn solve_eq(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&mut frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Equal)) => next.solve(frame),
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub(super) fn solve_neq(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&mut frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Equal)) => Response::Fail,
        Ok(_) => next.solve(frame),
        Err(r) => r,
    }
}

pub(super) fn solve_lss(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&mut frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Less)) => next.solve(frame),
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub(super) fn solve_leq(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&mut frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Less)) | Ok(Some(core::cmp::Ordering::Equal)) => {
            next.solve(frame)
        }
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub(super) fn solve_gtr(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&mut frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Greater)) => next.solve(frame),
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub(super) fn solve_geq(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&mut frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Greater)) | Ok(Some(core::cmp::Ordering::Equal)) => {
            next.solve(frame)
        }
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

enum Value {
    Integer(i64),
    Float(f64),
}

impl Value {
    fn to_float(&self, location: &Option<stream::Span>) -> Result<f64, Response> {
        match self {
            Value::Integer(i) => {
                let f = *i as f64;
                if *i != f as i64 {
                    Err(throw::error(
                        read_term::Term::new_compound(
                            "evaluation_error".to_string(),
                            None,
                            vec![read_term::Term::new_atom(
                                "float_overflow".to_string(),
                                None,
                            )],
                        ),
                        location.clone(),
                    ))
                } else {
                    Ok(f)
                }
            }
            Value::Float(f) => Ok(*f),
        }
    }
}

fn eval(frame: &Frame, expr: usize) -> Result<Value, Response> {
    match frame.get_term(expr) {
        Term::Term(t) => match &t.kind {
            read_term::TermKind::Integer(i) => Ok(Value::Integer(*i)),
            read_term::TermKind::Float(d) => Ok(Value::Float(*d)),
            read_term::TermKind::Atom(s) => match get_builtin(&format!("{}/0", s)) {
                Some(f) => {
                    (f)(&[],&t.location)
                }
                None => Err(throw_evaluable(t)),
            },
            _ => unreachable!(),
        },
        Term::Var(idx) => {
            if let Some(expr) = frame.get_var(*idx) {
                eval(frame, expr)
            } else {
                Err(throw::instantiation_error(frame))
            }
        }
        Term::Compound(c) => match get_builtin(&format!("{}/{}", c.functor(), c.args.len())) {
            Some(f) => {
                let mut args = Vec::new();
                for a in c.args.iter() {
                    args.push(eval(frame, *a)?);
                }
                (f)(&args,&c.compound.location)
            }
            None => Err(throw_evaluable(&c.compound)),
        },
    }
}

pub(super) fn solve_is(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match eval(&mut frame, args[1]) {
        Err(r) => r,
        Ok(Value::Integer(i)) => frame.sub_frame(|mut frame| {
            let expr = frame.new_term(&read_term::Term::new_integer(i, None));
            if frame.unify(args[0], expr) {
                next.solve(frame)
            } else {
                Response::Fail
            }
        }),
        Ok(Value::Float(d)) => frame.sub_frame(|mut frame| {
            let expr = frame.new_term(&read_term::Term::new_float(d, None));
            if frame.unify(args[0], expr) {
                next.solve(frame)
            } else {
                Response::Fail
            }
        }),
    }
}

fn promote2<R, FI: FnOnce(i64, i64) -> R, FD: FnOnce(f64, f64) -> R>(
    v1: &Value,
    v2: &Value,
    fi: FI,
    fd: FD,
    location: &Option<stream::Span>,
) -> Result<R, Response> {
    match (v1, v2) {
        (Value::Integer(i1), Value::Integer(i2)) => Ok((fi)(*i1, *i2)),
        (Value::Integer(_), Value::Float(f2)) => Ok((fd)(v1.to_float(location)?, *f2)),
        (Value::Float(f1), Value::Integer(_)) => Ok((fd)(*f1, v2.to_float(location)?)),
        (Value::Float(f1), Value::Float(f2)) => Ok((fd)(*f1, *f2)),
    }
}

fn eval_sub(args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response> {
    promote2(
        &args[0],
        &args[1],
        |a, b| Value::Integer(a - b),
        |a, b| Value::Float(a - b),
        location,
    )
}

fn not_impl(args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response> {
    if let Some(location) = location {
        eprintln!(
            "unimplemented evaluable function at: {}:{}:{}",
            location.start.source, location.start.line, location.start.column
        );
    }
    todo!()
}

type EvalFn = fn(args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response>;

const BUILTINS: phf::Map<&'static str, EvalFn> = phf_map! {
    "+/2" => not_impl,
    "-/2" => eval_sub,
    "*/2" => not_impl,
    "///2" => not_impl,
    "//2" => not_impl,
    "rem/2" => not_impl,
    "mod/2" => not_impl,
    "-/1" => not_impl,
    "abs/1" => not_impl,
    "sign/1" => not_impl,
    "float_integer_part/1" => not_impl,
    "float_fractional_part/1" => not_impl,
    "float/1" => not_impl,
    "floor/1" => not_impl,
    "truncate/1" => not_impl,
    "round/1" => not_impl,
    "ceiling/1" => not_impl
};

fn get_builtin(pi: &str) -> Option<&EvalFn> {
    BUILTINS.get(pi)
}
