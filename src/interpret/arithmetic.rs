use phf::phf_map;

use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

fn throw_evaluable(term: &Rc<read_term::Term>) -> Response {
    throw::error(
        read_term::Term::new_compound(
            "evaluable".to_string(),
            None,
            vec![term.as_pi().unwrap_or(term.clone())],
        ),
        term.location.clone(),
    )
}

fn compare_with_overflow(location: &Option<stream::Span>, i: &i64) -> Result<f64, Response> {
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

fn compare(frame: &Frame, t1: usize, t2: usize) -> Result<Option<core::cmp::Ordering>, Response> {
    let (t1, _) = frame.get_term(t1);
    let (t2, _) = frame.get_term(t2);
    match (&t1.kind, &t1.source.kind, &t2.kind, &t2.source.kind) {
        (
            TermKind::Atomic,
            read_term::TermKind::Integer(i1),
            TermKind::Atomic,
            read_term::TermKind::Integer(i2),
        ) => Ok(core::cmp::PartialOrd::partial_cmp(i1, i2)),
        (
            TermKind::Atomic,
            read_term::TermKind::Integer(i1),
            TermKind::Atomic,
            read_term::TermKind::Float(d2),
        ) => Ok(core::cmp::PartialOrd::partial_cmp(
            &compare_with_overflow(&t1.source.location, i1)?,
            d2,
        )),
        (
            TermKind::Atomic,
            read_term::TermKind::Float(d1),
            TermKind::Atomic,
            read_term::TermKind::Integer(i2),
        ) => Ok(core::cmp::PartialOrd::partial_cmp(
            d1,
            &compare_with_overflow(&t2.source.location, i2)?,
        )),
        (
            TermKind::Atomic,
            read_term::TermKind::Float(d1),
            TermKind::Atomic,
            read_term::TermKind::Float(d2),
        ) => Ok(core::cmp::PartialOrd::partial_cmp(d1, d2)),
        (TermKind::Atomic, _, _, _) => Err(throw_evaluable(&t1.source)),
        (_, _, TermKind::Atomic, _) => Err(throw_evaluable(&t2.source)),
        (TermKind::Var(_), _, _, _) => Err(throw::instantiation_error(&t1.source)),
        (_, _, TermKind::Var(_), _) => Err(throw::instantiation_error(&t2.source)),
        (TermKind::Compound(_), _, _, _) => Err(throw_evaluable(&t1.source)),
        (_, _, TermKind::Compound(_), _) => Err(throw_evaluable(&t2.source)),
    }
}

pub fn solve_eq(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Equal)) => next.solve(frame),
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub fn solve_neq(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Equal)) => Response::Fail,
        Ok(_) => next.solve(frame),
        Err(r) => r,
    }
}

pub fn solve_lss(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Less)) => next.solve(frame),
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub fn solve_leq(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Less)) | Ok(Some(core::cmp::Ordering::Equal)) => {
            next.solve(frame)
        }
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub fn solve_gtr(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&frame, args[0], args[1]) {
        Ok(Some(core::cmp::Ordering::Greater)) => next.solve(frame),
        Ok(_) => Response::Fail,
        Err(r) => r,
    }
}

pub fn solve_geq(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match compare(&frame, args[0], args[1]) {
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
    let (expr, _) = frame.get_term(expr);
    match (&expr.kind, &expr.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Integer(i)) => Ok(Value::Integer(*i)),
        (TermKind::Atomic, read_term::TermKind::Float(f)) => Ok(Value::Float(*f)),
        (TermKind::Atomic, read_term::TermKind::Atom(s)) => {
            match get_builtin(&format!("{}/0", s)) {
                Some(f) => (f)(&[], &expr.source.location),
                None => Err(throw_evaluable(&expr.source)),
            }
        }
        (TermKind::Var(_), read_term::TermKind::Var(_)) => {
            Err(throw::instantiation_error(&expr.source))
        }
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            match get_builtin(&format!("{}/{}", c.functor, args.len())) {
                Some(f) => {
                    let mut values = Vec::new();
                    for a in args.iter() {
                        values.push(eval(frame, *a)?);
                    }
                    (f)(&values, &expr.source.location)
                }
                None => Err(throw_evaluable(&expr.source)),
            }
        }
        _ => unreachable!(),
    }
}

pub fn solve_is(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    match eval(&frame, args[1]) {
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
