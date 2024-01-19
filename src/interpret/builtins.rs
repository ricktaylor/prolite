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
    if let Term::Compound(c) = frame.get_term(args[0]) {
        if c.functor() == "->" && c.args.len() == 2 {
            let if_term = c.args[0];
            let then_term = c.args[1];
            return solve_if_then_else(frame, if_term, then_term, Some(args[1]), next);
        }
    }

    frame
        .sub_frame(|frame| solve(frame, args[0], next))
        .map_failed(|| frame.sub_frame(|frame| solve(frame, args[1], next)))
}

fn solve_if(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    solve_if_then_else(frame, args[0], args[1], None, next)
}

fn solve_if_then_else(
    mut frame: Frame,
    if_term: usize,
    then_term: usize,
    else_term: Option<usize>,
    next: &mut dyn Solver,
) -> Response {
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
            let a = frame.new_term(ball.clone());
            if frame.unify(a, args[1]) {
                solve::call(frame, args[2], next)
            } else {
                Response::Throw(ball)
            }
        }),
        r => r,
    }
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

fn solve_unify(mut frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    frame.sub_frame(|mut frame| {
        if frame.unify(args[0], args[1]) {
            next.solve(frame)
        } else {
            Response::Fail
        }
    })
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

fn solve_asserta(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    user_defined::assert(frame, args[0], false, next)
}

fn solve_assertz(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    user_defined::assert(frame, args[0], true, next)
}

fn solve_current_char_conversion(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    flags::solve_current_char_conversion(frame, args[0], args[1], next)
}

fn not_impl(frame: Frame, _: &[usize], _: &mut dyn Solver) -> Response {
    if let Some(location) = frame.get_location() {
        eprintln!(
            "unimplemented builtin at: {}:{}:{}",
            location.start.source, location.start.line, location.start.column
        );
    }
    todo!()
}

type SolveFn = fn(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response;

const BUILTINS: phf::Map<&'static str, (SolveFn, bool)> = phf_map! {
    "true/0" => (solve_true,true),
    "fail/0" => (solve_fail,true),
    "call/1" => (solve_call,false),
    "!/0" => (solve_cut,true),
    ",/2" => (solve_and,true),
    ";/2" => (solve_or,true),
    "->/2" => (solve_if,true),
    "catch/3" => (solve_catch,false),
    "throw/1" => (throw::solve,false),
    "=/2" => (solve_unify,false),
    "unify_with_occurs_check/2" => (not_impl,false),
    "\\=/2" => (not_impl,false),
    "var/1" => (not_impl,false),
    "atom/1" => (not_impl,false),
    "integer/1" => (not_impl,false),
    "float/1" => (not_impl,false),
    "atomic/1" => (not_impl,false),
    "compound/1" => (not_impl,false),
    "nonvar/1" => (not_impl,false),
    "number/1" => (not_impl,false),
    "@=</2" => (not_impl,false),
    "==/2" => (not_impl,false),
    "\\==/2" => (not_impl,false),
    "@</2" => (not_impl,false),
    "@>/2" => (not_impl,false),
    "@>=/2" => (not_impl,false),
    "functor/3" => (not_impl,false),
    "arg/3" => (not_impl,false),
    "=../2" => (univ::solve,false),
    "copy_term/2" => (solve_copy_term,false),
    "is/2" => (arithmetic::solve_is,false),
    "=:=/2" => (arithmetic::solve_eq,false),
    "=\\=/2" => (arithmetic::solve_neq,false),
    "</2" => (arithmetic::solve_lss,false),
    "=</2" => (arithmetic::solve_leq,false),
    ">/2" => (arithmetic::solve_gtr,false),
    ">=/2" => (arithmetic::solve_geq,false),
    "clause/2" => (not_impl,false),
    "current_predicate/1" => (not_impl,false),
    "asserta/1" => (solve_asserta,false),
    "assertz/1" => (solve_assertz,false),
    "retract/1" => (not_impl,false),
    "abolish/1" => (not_impl,false),
    "findall/3" => (findall::solve,false),
    "bagof/3" => (not_impl,false),
    "setof/3" => (setof::solve,false),
    "current_input/1" => (not_impl,false),
    "current_output/1" => (not_impl,false),
    "set_input/1" => (not_impl,false),
    "set_output/1" => (not_impl,false),
    "open/4" => (not_impl,false),
    "open/3" => (not_impl,false),
    "close/2" => (not_impl,false),
    "close/1" => (not_impl,false),
    "flush_output/1" => (not_impl,false),
    "flush_output/0" => (not_impl,false),
    "stream_property/2" => (not_impl,false),
    "at_end_of_stream/0" => (not_impl,false),
    "at_end_of_stream/1" => (not_impl,false),
    "set_stream_position/2" => (not_impl,false),
    "get_char/2" => (not_impl,false),
    "get_char/1" => (not_impl,false),
    "get_code/1" => (not_impl,false),
    "get_code/2" => (not_impl,false),
    "peek_char/2" => (not_impl,false),
    "peek_char/1" => (not_impl,false),
    "peek_code/1" => (not_impl,false),
    "peek_code/2" => (not_impl,false),
    "put_char/2" => (not_impl,false),
    "put_char/1" => (not_impl,false),
    "put_code/1" => (not_impl,false),
    "put_code/2" => (not_impl,false),
    "nl/0" => (write::solve_nl,false),
    "nl/1" => (not_impl,false),
    "get_byte/2" => (not_impl,false),
    "get_byte/1" => (not_impl,false),
    "peek_byte/2" => (not_impl,false),
    "peek_byte/1" => (not_impl,false),
    "put_byte/2" => (not_impl,false),
    "put_byte/1" => (not_impl,false),
    "read_term/3" => (not_impl,false),
    "read_term/2" => (not_impl,false),
    "read/1" => (not_impl,false),
    "read/2" => (not_impl,false),
    "write_term/3" => (not_impl,false),
    "write_term/2" => (not_impl,false),
    "write/1" => (write::solve_write1,false),
    "write/2" => (not_impl,false),
    "writeq/1" => (not_impl,false),
    "writeq/2" => (not_impl,false),
    "write_canonical/1" => (not_impl,false),
    "write_canonical/2" => (not_impl,false),
    "op/3" => (not_impl,false),
    "current_op/3" => (not_impl,false),
    "char_conversion/2" => (not_impl,false),
    "current_char_conversion/2" => (solve_current_char_conversion,false),
    "\\+/1" => (solve_not_provable,false),
    "once/1" => (not_impl,false),
    "repeat/0" => (solve_repeat,false),
    "atom_length/2" => (not_impl,false),
    "atom_concat/3" => (not_impl,false),
    "sub_atom/5" => (not_impl,false),
    "atom_chars/2" => (not_impl,false),
    "atom_codes/2" => (not_impl,false),
    "char_code/2" => (not_impl,false),
    "number_chars/2" => (not_impl,false),
    "number_codes/2" => (not_impl,false),
    "set_prolog_flag/2" => (not_impl,false),
    "current_prolog_flag/2" => (not_impl,false),
    "halt/0" => (not_impl,false),
    "halt/1" => (not_impl,false),
};

pub fn get_builtin(pi: &str) -> Option<&(SolveFn, bool)> {
    let mut r = BUILTINS.get(pi);
    if r.is_none() && pi.starts_with("call/") {
        r = Some(&(not_impl, true))
    }
    r
}
