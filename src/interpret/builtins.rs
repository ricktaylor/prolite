use std::todo;

use phf::phf_map;

use super::*;
use solve::solve;
use term::*;

fn solve_true(
    ctx: &mut Context,
    _: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    next.solve(ctx, substs)
}

fn solve_fail(_: &mut Context, _: &[Rc<Term>], _: &[Var], _: &mut dyn Solver) -> Response {
    Response::Fail
}

fn solve_call(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve::call(ctx, &args[0], substs, next)
}

fn solve_cut(ctx: &mut Context, _: &[Rc<Term>], substs: &[Var], next: &mut dyn Solver) -> Response {
    next.solve(ctx, substs).map_failed(|| Response::Cut)
}

fn solve_and(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve(
        ctx,
        &args[0],
        substs,
        &mut Continuation::new(|ctx, substs| solve(ctx, &args[1], substs, next)),
    )
}

fn solve_or(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve(ctx, &args[0], substs, next).map_failed(|| solve(ctx, &args[1], substs, next))
}

fn solve_if(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    let (if_term, then_term, else_term) = match &args[1].kind {
        TermKind::Compound(c) if c.functor == ";" && c.args.len() == 2 => {
            (&args[0], &c.args[0], Some(&c.args[1]))
        }
        _ => (&args[0], &args[1], None),
    };

    let mut if_true = false;
    let mut then_cut = false;
    solve(
        ctx,
        if_term,
        substs,
        &mut Continuation::new(|ctx, substs| {
            if_true = true;
            solve(ctx, then_term, substs, next)
                .map_cut(|| {
                    then_cut = true;
                    Response::Cut
                })
                .map_failed(|| Response::Cut)
        }),
    )
    .map_cut(|| {
        if then_cut {
            Response::Cut
        } else {
            Response::Fail
        }
    })
    .map_failed(|| match else_term {
        Some(else_term) if !if_true => solve(ctx, else_term, substs, next),
        _ => Response::Fail,
    })
}

fn solve_catch(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    let mut next_throw = false;
    match solve::call(
        ctx,
        &args[0],
        substs,
        &mut Continuation::new(|ctx, substs| match next.solve(ctx, substs) {
            Response::Throw(t) => {
                next_throw = true;
                Response::Throw(t)
            }
            r => r,
        }),
    ) {
        Response::Throw(ball) if !next_throw => {
            match solve::unify(&ball, &args[1], substs.to_vec()) {
                Err(_) => Response::Throw(ball),
                Ok(substs) => solve::call(ctx, &args[2], &substs, next),
            }
        }
        r => r,
    }
}

fn solve_throw(_: &mut Context, args: &[Rc<Term>], substs: &[Var], _: &mut dyn Solver) -> Response {
    // Dereference ball
    let mut ball = &args[0];
    while let term::TermKind::Var(idx) = &ball.kind {
        if let Some(t) = substs[*idx] {
            ball = t;
        } else {
            // Instantiation error!
            todo!()
        }
    }
    Response::Throw(ball.clone())
}

fn solve_not_provable(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve::call(
        ctx,
        &args[0],
        substs,
        &mut Continuation::new(|_, _| Response::Fail),
    )
    .map_failed(|| next.solve(ctx, substs))
}

fn solve_once(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve::call(
        ctx,
        &Term::new_compound(
            ",".to_string(),
            stream::Span::default(),
            vec![
                args[0].clone(),
                Term::new_atom("!".to_string(), stream::Span::default()),
            ],
        ),
        substs,
        next,
    )
}

fn solve_repeat(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    loop {
        match next.solve(ctx, substs) {
            Response::Fail => {}
            r => break r,
        }
    }
}

fn solve_unify(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve::unify(&args[0], &args[1], substs.to_vec())
        .map_or_else(|r| r, |substs| next.solve(ctx, &substs))
}

fn not_impl(_: &mut Context, _: &[Rc<Term>], _: &[Var], _: &mut dyn Solver) -> Response {
    todo!()
}

type SolveFn =
    fn(ctx: &mut Context, args: &[Rc<Term>], substs: &[Var], next: &mut dyn Solver) -> Response;

static BUILTINS: phf::Map<&'static str, SolveFn> = phf_map! {
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
    "copy_term/2" => copy_term::solve,
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
    "setof/3" => findall::solve_setof,
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
    "once/1" => solve_once,
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
