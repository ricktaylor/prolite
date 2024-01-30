use phf::phf_map;

use super::*;
use frame::Frame;
use solve::Solver;
use term::TermKind;

fn throw_evaluable(term: &Rc<read_term::Term>) -> Response {
    throw::error(
        read_term::Term::new_compound(
            "type_error".to_string(),
            None,
            vec![
                read_term::Term::new_atom("evaluable".to_string(), None),
                term.as_pi().unwrap_or(term.clone()),
            ],
        ),
        term.location.clone(),
    )
}

enum ValueKind {
    Integer(i64),
    Float(f64),
}

struct Value {
    kind: ValueKind,
    location: Option<stream::Span>,
}

impl Value {
    fn new_integer(i: i64, location: &Option<stream::Span>) -> Self {
        Self {
            kind: ValueKind::Integer(i),
            location: location.clone(),
        }
    }

    fn new_float(f: f64, location: &Option<stream::Span>) -> Self {
        Self {
            kind: ValueKind::Float(f),
            location: location.clone(),
        }
    }

    fn to_float(&self) -> Result<f64, Response> {
        match &self.kind {
            ValueKind::Integer(i) => {
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
                        self.location.clone(),
                    ))
                } else {
                    Ok(f)
                }
            }
            ValueKind::Float(f) => Ok(*f),
        }
    }
}

fn eval(frame: &Frame, expr: usize) -> Result<Value, Response> {
    let (expr, _) = frame.get_term(expr);
    match (&expr.kind, &expr.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Integer(i)) => {
            Ok(Value::new_integer(*i, &expr.source.location))
        }
        (TermKind::Atomic, read_term::TermKind::Float(f)) => {
            Ok(Value::new_float(*f, &expr.source.location))
        }
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
        Ok(v) => match &v.kind {
            ValueKind::Integer(i) => frame.sub_frame(|mut frame| {
                let expr = frame.new_term(&read_term::Term::new_integer(*i, v.location));
                if frame.unify(args[0], expr) {
                    next.solve(frame)
                } else {
                    Response::Fail
                }
            }),
            ValueKind::Float(f) => frame.sub_frame(|mut frame| {
                let expr = frame.new_term(&read_term::Term::new_float(*f, v.location));
                if frame.unify(args[0], expr) {
                    next.solve(frame)
                } else {
                    Response::Fail
                }
            }),
        },
    }
}

fn promote2<R, FI: FnOnce(i64, i64) -> R, FD: FnOnce(f64, f64) -> R>(
    v1: &Value,
    v2: &Value,
    fi: FI,
    fd: FD,
) -> Result<R, Response> {
    match (&v1.kind, &v2.kind) {
        (ValueKind::Integer(i1), ValueKind::Integer(i2)) => Ok((fi)(*i1, *i2)),
        (ValueKind::Integer(_), ValueKind::Float(f2)) => Ok((fd)(v1.to_float()?, *f2)),
        (ValueKind::Float(f1), ValueKind::Integer(_)) => Ok((fd)(*f1, v2.to_float()?)),
        (ValueKind::Float(f1), ValueKind::Float(f2)) => Ok((fd)(*f1, *f2)),
    }
}

fn compare(frame: &Frame, t1: usize, t2: usize) -> Result<Option<core::cmp::Ordering>, Response> {
    let e1 = eval(frame, t1)?;
    let e2 = eval(frame, t2)?;
    promote2(&e1, &e2, |a, b| a.partial_cmp(&b), |a, b| a.partial_cmp(&b))
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

fn eval_add(args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response> {
    promote2(
        &args[0],
        &args[1],
        |a, b| match a.checked_add(b) {
            None => {
                todo!()
            }
            Some(c) => Value::new_integer(c, location),
        },
        |a, b| Value::new_float(a + b, location),
    )
}

fn eval_sub(args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response> {
    promote2(
        &args[0],
        &args[1],
        |a, b| match a.checked_sub(b) {
            None => {
                todo!()
            }
            Some(c) => Value::new_integer(c, location),
        },
        |a, b| Value::new_float(a - b, location),
    )
}

fn eval_mul(args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response> {
    promote2(
        &args[0],
        &args[1],
        |a, b| match a.checked_mul(b) {
            None => {
                todo!()
            }
            Some(c) => Value::new_integer(c, location),
        },
        |a, b| Value::new_float(a * b, location),
    )
}

fn not_impl(_args: &[Value], location: &Option<stream::Span>) -> Result<Value, Response> {
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
    "+/2" => eval_add,
    "-/2" => eval_sub,
    "*/2" => eval_mul,
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
