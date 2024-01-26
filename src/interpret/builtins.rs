use phf::phf_map;

use super::*;
use frame::Frame;
use solve::{Continuation, GenerateFn, SolveFn, Solver};
use term::TermKind;

pub enum Builtin {
    Control(GenerateFn),
    Solve(SolveFn),
}

fn solve_call(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    solve::call(frame, args[0], next)
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
    //eprintln!("copy_term({},{})",write::write_term(&frame, args[0]),write::write_term(&frame, args[1]));
    frame.sub_frame(|mut frame| {
        let t = frame.copy_term(args[0]);
        if frame.unify(t, args[1]) {
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

fn solve_var(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (term, _) = frame.get_term(args[0]);
    match &term.kind {
        TermKind::Var(_) => next.solve(frame),
        _ => Response::Fail,
    }
}

pub fn not_impl(_: Frame, _: &[usize], _: &mut dyn Solver) -> Response {
    todo!()
}

const BUILTINS: phf::Map<&'static str, Builtin> = phf_map! {
    "true/0" => Builtin::Control(solve::generate_true),
    "fail/0" => Builtin::Control(solve::generate_fail),
    "!/0" => Builtin::Control(solve::generate_cut),
    ",/2" => Builtin::Control(solve::generate_and),
    ";/2" => Builtin::Control(solve::generate_or),
    "->/2" => Builtin::Control(solve::generate_if_then),
    "call/1" => Builtin::Solve(solve_call),
    "catch/3" => Builtin::Solve(throw::solve_catch),
    "throw/1" => Builtin::Solve(throw::solve_throw),
    "=/2" => Builtin::Solve(solve_unify),
    "unify_with_occurs_check/2" => Builtin::Solve(not_impl),
    "\\=/2" => Builtin::Solve(not_impl),
    "var/1" => Builtin::Solve(solve_var),
    "atom/1" => Builtin::Solve(not_impl),
    "integer/1" => Builtin::Solve(not_impl),
    "float/1" => Builtin::Solve(not_impl),
    "atomic/1" => Builtin::Solve(not_impl),
    "compound/1" => Builtin::Solve(not_impl),
    "nonvar/1" => Builtin::Solve(not_impl),
    "number/1" => Builtin::Solve(not_impl),
    "@=</2" => Builtin::Solve(not_impl),
    "==/2" => Builtin::Solve(not_impl),
    "\\==/2" => Builtin::Solve(not_impl),
    "@</2" => Builtin::Solve(not_impl),
    "@>/2" => Builtin::Solve(not_impl),
    "@>=/2" => Builtin::Solve(not_impl),
    "functor/3" => Builtin::Solve(not_impl),
    "arg/3" => Builtin::Solve(not_impl),
    "=../2" => Builtin::Solve(univ::solve),
    "copy_term/2" => Builtin::Solve(solve_copy_term),
    "is/2" => Builtin::Solve(arithmetic::solve_is),
    "=:=/2" => Builtin::Solve(arithmetic::solve_eq),
    "=\\=/2" => Builtin::Solve(arithmetic::solve_neq),
    "</2" => Builtin::Solve(arithmetic::solve_lss),
    "=</2" => Builtin::Solve(arithmetic::solve_leq),
    ">/2" => Builtin::Solve(arithmetic::solve_gtr),
    ">=/2" => Builtin::Solve(arithmetic::solve_geq),
    "clause/2" => Builtin::Solve(not_impl),
    "current_predicate/1" => Builtin::Solve(not_impl),
    "asserta/1" => Builtin::Solve(solve_asserta),
    "assertz/1" => Builtin::Solve(solve_assertz),
    "retract/1" => Builtin::Solve(not_impl),
    "abolish/1" => Builtin::Solve(not_impl),
    "findall/3" => Builtin::Solve(findall::findall),
    "bagof/3" => Builtin::Solve(not_impl),
    "setof/3" => Builtin::Solve(findall::setof),
    "current_input/1" => Builtin::Solve(not_impl),
    "current_output/1" => Builtin::Solve(not_impl),
    "set_input/1" => Builtin::Solve(not_impl),
    "set_output/1" => Builtin::Solve(not_impl),
    "open/4" => Builtin::Solve(not_impl),
    "open/3" => Builtin::Solve(not_impl),
    "close/2" => Builtin::Solve(not_impl),
    "close/1" => Builtin::Solve(not_impl),
    "flush_output/1" => Builtin::Solve(not_impl),
    "flush_output/0" => Builtin::Solve(not_impl),
    "stream_property/2" => Builtin::Solve(not_impl),
    "at_end_of_stream/0" => Builtin::Solve(not_impl),
    "at_end_of_stream/1" => Builtin::Solve(not_impl),
    "set_stream_position/2" => Builtin::Solve(not_impl),
    "get_char/2" => Builtin::Solve(not_impl),
    "get_char/1" => Builtin::Solve(not_impl),
    "get_code/1" => Builtin::Solve(not_impl),
    "get_code/2" => Builtin::Solve(not_impl),
    "peek_char/2" => Builtin::Solve(not_impl),
    "peek_char/1" => Builtin::Solve(not_impl),
    "peek_code/1" => Builtin::Solve(not_impl),
    "peek_code/2" => Builtin::Solve(not_impl),
    "put_char/2" => Builtin::Solve(not_impl),
    "put_char/1" => Builtin::Solve(not_impl),
    "put_code/1" => Builtin::Solve(not_impl),
    "put_code/2" => Builtin::Solve(not_impl),
    "nl/0" => Builtin::Solve(write::solve_nl0),
    "nl/1" => Builtin::Solve(not_impl),
    "get_byte/2" => Builtin::Solve(not_impl),
    "get_byte/1" => Builtin::Solve(not_impl),
    "peek_byte/2" => Builtin::Solve(not_impl),
    "peek_byte/1" => Builtin::Solve(not_impl),
    "put_byte/2" => Builtin::Solve(not_impl),
    "put_byte/1" => Builtin::Solve(not_impl),
    "read_term/3" => Builtin::Solve(not_impl),
    "read_term/2" => Builtin::Solve(not_impl),
    "read/1" => Builtin::Solve(not_impl),
    "read/2" => Builtin::Solve(not_impl),
    "write_term/3" => Builtin::Solve(not_impl),
    "write_term/2" => Builtin::Solve(not_impl),
    "write/1" => Builtin::Solve(write::solve_write1),
    "write/2" => Builtin::Solve(not_impl),
    "writeq/1" => Builtin::Solve(not_impl),
    "writeq/2" => Builtin::Solve(not_impl),
    "write_canonical/1" => Builtin::Solve(not_impl),
    "write_canonical/2" => Builtin::Solve(not_impl),
    "op/3" => Builtin::Solve(not_impl),
    "current_op/3" => Builtin::Solve(not_impl),
    "char_conversion/2" => Builtin::Solve(not_impl),
    "current_char_conversion/2" => Builtin::Solve(solve_current_char_conversion),
    "\\+/1" => Builtin::Solve(solve_not_provable),
    "once/1" => Builtin::Solve(not_impl),
    "repeat/0" => Builtin::Solve(solve_repeat),
    "atom_length/2" => Builtin::Solve(not_impl),
    "atom_concat/3" => Builtin::Solve(not_impl),
    "sub_atom/5" => Builtin::Solve(not_impl),
    "atom_chars/2" => Builtin::Solve(not_impl),
    "atom_codes/2" => Builtin::Solve(not_impl),
    "char_code/2" => Builtin::Solve(not_impl),
    "number_chars/2" => Builtin::Solve(not_impl),
    "number_codes/2" => Builtin::Solve(not_impl),
    "set_prolog_flag/2" => Builtin::Solve(not_impl),
    "current_prolog_flag/2" => Builtin::Solve(not_impl),
    "halt/0" => Builtin::Solve(not_impl),
    "halt/1" => Builtin::Solve(not_impl)
};

pub fn get_builtin(pi: &str) -> Option<&Builtin> {
    let mut r = BUILTINS.get(pi);
    if r.is_none() && pi.starts_with("call/") {
        r = Some(&Builtin::Solve(not_impl));
    }
    r
}
