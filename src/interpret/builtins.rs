use std::todo;

use phf::phf_map;

use super::*;
use solve::{solve, Continuation, Frame, Solver};
use term::*;

fn solve_true(frame: Frame, _: &[usize], next: &mut dyn Solver) -> Response {
    next.solve(frame)
}

fn solve_fail(_: Frame, _: &[usize], _: &mut dyn Solver) -> Response {
    Response::Fail
}

fn solve_call(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    solve::call(frame, args[0], next)
}

fn solve_cut(frame: Frame, _: &[usize], next: &mut dyn Solver) -> Response {
    next.solve(frame).map_failed(|| Response::Cut)
}

fn solve_and(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    solve(
        frame,
        args[0],
        &mut Continuation::new(|frame| solve(frame, args[1], next)),
    )
}

fn solve_or(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    frame
        .sub_frame(|frame| solve(frame, args[0], next))
        .map_failed(|| frame.sub_frame(|frame| solve(frame, args[1], next)))
}

fn solve_if(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (if_term, then_term, else_term) = match frame.get_term(args[1]) {
        Term::Compound(c) if c.functor() == ";" && c.args.len() == 2 => {
            (args[0], c.args[0], Some(c.args[1]))
        }
        _ => (args[0], args[1], None),
    };

    let mut if_true = false;
    let mut then_cut = false;
    frame
        .sub_frame(|frame| {
            solve(
                frame,
                if_term,
                &mut Continuation::new(|frame| {
                    if_true = true;
                    solve(frame, then_term, next)
                        .map_cut(|| {
                            then_cut = true;
                            Response::Cut
                        })
                        .map_failed(|| Response::Cut)
                }),
            )
        })
        .map_cut(|| {
            if then_cut {
                Response::Cut
            } else {
                Response::Fail
            }
        })
        .map_failed(|| match else_term {
            Some(else_term) if !if_true => solve(frame, else_term, next),
            _ => Response::Fail,
        })
}

fn solve_catch(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
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

fn solve_throw_var_check(frame: &Frame, ball: usize) -> Result<Rc<read_term::Term>, Response> {
    match frame.get_term(ball) {
        Term::Term(t) => Ok(t.clone()),
        Term::Var(v) => todo!(),
        Term::Compound(c) => {
            for a in c.args.iter() {
                solve_throw_var_check(frame, *a)?;
            }
            Ok(c.compound.clone())
        }
    }
}

fn solve_throw(frame: Frame, args: &[usize], _: &mut dyn Solver) -> Response {
    solve_throw_var_check(&frame, args[0]).map_or_else(|r| r, |ball| Response::Throw(ball.clone()))
}

fn solve_not_provable(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    frame
        .sub_frame(|frame| solve::call(frame, args[0], &mut Continuation::new(|_| Response::Fail)))
        .map_failed(|| next.solve(frame))
}

/*fn solve_once(frame: Frame args: &[usize], next: &mut dyn Solver) -> Response {
    solve::call(
        frame,
        &read_term::new_compound(
            ",".to_string(),
            stream::Span::default(),
            vec![
                args[0].clone(),
                read_term::new_atom("!".to_string(), stream::Span::default()),
            ],
        ),
        next,
    )
}*/

fn solve_repeat(mut frame: Frame, _: &[usize], next: &mut dyn Solver) -> Response {
    loop {
        match frame.sub_frame(|frame| next.solve(frame)) {
            Response::Fail => {}
            r => break r,
        }
    }
}

fn solve_unify(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    solve::unify(frame, args[0], args[1], next)
}

fn solve_copy_term(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    frame.sub_frame(|mut frame| {
        if frame.unify_copy(args[0], args[1]) {
            next.solve(frame)
        } else {
            Response::Fail
        }
    })
}

fn not_impl(_: Frame, _: &[usize], _: &mut dyn Solver) -> Response {
    todo!()
}

type SolveFn = fn(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response;

const BUILTINS: phf::Map<&'static str, SolveFn> = phf_map! {
    "true/0" => solve_true,
    "fail/0" => solve_fail,
    "call/1" => solve_call,
    "!/0" => solve_cut,
    ",/2" => solve_and,
    ";/2" => solve_or,
    "->/2" => solve_if,
    "catch/3" => solve_catch,
    "throw/1" => solve_throw,
    "=/2" => solve_unify,
    "unify_with_occurs_check/2" => not_impl,
    "\\=/2" => not_impl,
    "var/1" => not_impl,
    "atom/1" => not_impl,
    "integer/1" => not_impl,
    "float/1" => not_impl,
    "atomic/1" => not_impl,
    "compound/1" => not_impl,
    "nonvar/1" => not_impl,
    "number/1" => not_impl,
    "@=</2" => not_impl,
    "==/2" => not_impl,
    "\\==/2" => not_impl,
    "@</2" => not_impl,
    "@>/2" => not_impl,
    "@>=/2" => not_impl,
    "functor/3" => not_impl,
    "arg/3" => not_impl,
    "=../2" => not_impl,
    "copy_term/2" => solve_copy_term,
    "is/2" => not_impl,
    "=:=/2" => not_impl,
    "=\\=/2" => not_impl,
    "</2" => not_impl,
    "=</2" => not_impl,
    ">/2" => not_impl,
    ">=/2" => not_impl,
    "clause/2" => not_impl,
    "current_predicate/1" => not_impl,
    "asserta/1" => not_impl,
    "assertz/1" => not_impl,
    "retract/1" => not_impl,
    "abolish/1" => not_impl,
    "findall/3" => findall::solve_findall,
    "bagof/3" => not_impl,
    "setof/3" => setof::solve_setof,
    "current_input/1" => not_impl,
    "current_output/1" => not_impl,
    "set_input/1" => not_impl,
    "set_output/1" => not_impl,
    "open/4" => not_impl,
    "open/3" => not_impl,
    "close/2" => not_impl,
    "close/1" => not_impl,
    "flush_output/1" => not_impl,
    "flush_output/0" => not_impl,
    "stream_property/2" => not_impl,
    "at_end_of_stream/0" => not_impl,
    "at_end_of_stream/1" => not_impl,
    "set_stream_position/2" => not_impl,
    "get_char/2" => not_impl,
    "get_char/1" => not_impl,
    "get_code/1" => not_impl,
    "get_code/2" => not_impl,
    "peek_char/2" => not_impl,
    "peek_char/1" => not_impl,
    "peek_code/1" => not_impl,
    "peek_code/2" => not_impl,
    "put_char/2" => not_impl,
    "put_char/1" => not_impl,
    "put_code/1" => not_impl,
    "put_code/2" => not_impl,
    "nl/0" => write::solve_nl,
    "nl/1" => not_impl,
    "get_byte/2" => not_impl,
    "get_byte/1" => not_impl,
    "peek_byte/2" => not_impl,
    "peek_byte/1" => not_impl,
    "put_byte/2" => not_impl,
    "put_byte/1" => not_impl,
    "read_term/3" => not_impl,
    "read_term/2" => not_impl,
    "read/1" => not_impl,
    "read/2" => not_impl,
    "write_term/3" => not_impl,
    "write_term/2" => not_impl,
    "write/1" => write::solve_write,
    "write/2" => not_impl,
    "writeq/1" => not_impl,
    "writeq/2" => not_impl,
    "write_canonical/1" => not_impl,
    "write_canonical/2" => not_impl,
    "op/3" => not_impl,
    "current_op/3" => not_impl,
    "char_conversion/2" => not_impl,
    "current_char_conversion/2" => not_impl,
    "\\+/1" => solve_not_provable,
    "once/1" => not_impl,
    "repeat/0" => solve_repeat,
    "atom_length/2" => not_impl,
    "atom_concat/3" => not_impl,
    "sub_atom/5" => not_impl,
    "atom_chars/2" => not_impl,
    "atom_codes/2" => not_impl,
    "char_code/2" => not_impl,
    "number_chars/2" => not_impl,
    "number_codes/2" => not_impl,
    "set_prolog_flag/2" => not_impl,
    "current_prolog_flag/2" => not_impl,
    "halt/0" => not_impl,
    "halt/1" => not_impl,
};

pub(super) fn is_builtin(pi: &str) -> Option<&SolveFn> {
    BUILTINS.get(pi)
}
