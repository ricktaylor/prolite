use phf::phf_map;

use super::*;
use solve::solve;
use term::*;

struct Continuation<F: FnMut(&mut Context, &[Var]) -> Response> {
    solve: F,
}

impl<F> Solver for Continuation<F>
where
    F: FnMut(&mut Context, &[Var]) -> Response,
{
    fn solve(&mut self, ctx: &mut Context, substs: &[Var]) -> Response {
        (self.solve)(ctx, substs)
    }
}

impl<F> Continuation<F>
where
    F: FnMut(&mut Context, &[Var]) -> Response,
{
    fn new(f: F) -> Self {
        Self { solve: f }
    }
}

fn deref_var<'a>(mut term: &'a Term, substs: &'a [Var]) -> &'a Term {
    while let TermKind::Var(s) = &term.kind {
        match substs
            .binary_search_by(|v| v.name.cmp(s))
            .map_or_else(|_| panic!(), |idx| substs[idx].value)
        {
            None => break,
            Some(t) => term = t,
        }
    }
    term
}

fn solve_true(ctx: &mut Context, _: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
    next.solve(ctx, substs)
}

fn solve_fail(_: &mut Context, _: &[Term], _: &[Var], _: &mut dyn Solver) -> Response {
    Response::Fail
}

fn solve_call(ctx: &mut Context, args: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
    // call/1 is "not transparent" to cut, so use a continuation to record the response from next
    let mut next_cut = false;
    let close_cut = &mut Continuation::new(|c, s| {
        next.solve(c, s).map_cut(|| {
            next_cut = true;
            Response::Cut
        })
    });

    if let TermKind::Var(_) = &args[0].kind {
        // Convert variable to body
        solve(
            ctx,
            &deref_var(&args[0], substs).clone().into_goal(),
            substs,
            close_cut,
        )
    } else {
        solve(ctx, &args[0], substs, close_cut)
    }
    .map_cut(|| {
        if next_cut {
            Response::Cut
        } else {
            Response::Fail
        }
    })
}

fn solve_cut(ctx: &mut Context, _: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
    next.solve(ctx, substs).map_failed(|| Response::Cut)
}

fn solve_and(ctx: &mut Context, args: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
    solve(
        ctx,
        &args[0],
        substs,
        &mut Continuation::new(|c, s| solve(c, &args[1], s, next)),
    )
}

fn solve_or(ctx: &mut Context, args: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
    solve(ctx, &args[0], substs, next).map_failed(|| solve(ctx, &args[1], substs, next))
}

fn solve_throw(_: &mut Context, args: &[Term], substs: &[Var], _: &mut dyn Solver) -> Response {
    Response::Throw(deref_var(&args[0], substs).clone())
}

fn solve_if(ctx: &mut Context, args: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
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
        &mut Continuation::new(|c, s| {
            if_true = true;
            solve(c, then_term, s, next)
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

fn solve_not_provable(
    ctx: &mut Context,
    args: &[Term],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    solve_call(
        ctx,
        args,
        substs,
        &mut Continuation::new(|_, _| Response::Fail),
    )
    .map_failed(|| next.solve(ctx, substs))
}

fn solve_once(ctx: &mut Context, args: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response {
    // call/1 is "not transparent" to cut, so use a continuation to record the response from next
    let mut next_cut = false;
    let close_cut = &mut Continuation::new(|c, s| {
        next.solve(c, s).map_cut(|| {
            next_cut = true;
            Response::Cut
        })
    });

    // once/1 is called only once, so use a continuation to cut
    let once_cut =
        &mut Continuation::new(|c, s| close_cut.solve(c, s).map_failed(|| Response::Cut));

    if let TermKind::Var(_) = &args[0].kind {
        // Convert variable to body
        solve(
            ctx,
            &deref_var(&args[0], substs).clone().into_goal(),
            substs,
            once_cut,
        )
    } else {
        solve(ctx, &args[0], substs, once_cut)
    }
    .map_cut(|| {
        if next_cut {
            Response::Cut
        } else {
            Response::Fail
        }
    })
}

fn solve_repeat(
    ctx: &mut Context,
    args: &[Term],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    loop {
        let r = solve(ctx, &args[0], substs, next);
        match r {
            Response::Fail => {}
            _ => break r,
        }
    }
}

fn not_impl(_: &mut Context, _: &[Term], _: &[Var], _: &mut dyn Solver) -> Response {
    todo!()
}

type SolveFn =
    fn(ctx: &mut Context, args: &[Term], substs: &[Var], next: &mut dyn Solver) -> Response;

static BUILTINS: phf::Map<&'static str, SolveFn> = phf_map! {
    "true/0" => solve_true,
    "fail/0" => solve_fail,
    "call/1" => solve_call,
    "!/0" => solve_cut,
    ",/2" => solve_and,
    ";/2" => solve_or,
    "->/2" => solve_if,
    "catch/3" => not_impl,
    "throw/1" => solve_throw,
    "=/2" => not_impl,
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
    "copy_term/2" => not_impl,
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
    "findall/3" => not_impl,
    "bagof/3" => not_impl,
    "setof/3" => not_impl,
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
    "nl/0" => not_impl,
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
    "write/1" => not_impl,
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
