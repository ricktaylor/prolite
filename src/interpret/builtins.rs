use phf::phf_map;

use super::*;

#[derive(Clone)]
struct FnSolver<'a> {
    function: &'a fn(
        ctx: &mut Context,
        args: &[&term::Term],
        substs: &mut [Var],
        next: &dyn Solver,
    ) -> Response,
}

impl<'a> FnSolver<'a> {
    fn new(
        function: &'a fn(
            ctx: &mut Context,
            args: &[&term::Term],
            substs: &mut [Var],
            next: &dyn Solver,
        ) -> Response,
    ) -> Self {
        Self { function }
    }
}

impl<'a> Solver for FnSolver<'a> {
    fn call(
        &self,
        ctx: &mut Context,
        args: &[&term::Term],
        substs: &mut [Var],
        next: &dyn Solver,
    ) -> Response {
        (self.function)(ctx, args, substs, next)
    }
}

fn call_true(
    ctx: &mut Context,
    args: &[&term::Term],
    substs: &mut [Var],
    next: &dyn Solver,
) -> Response {
    next.call(ctx, args, substs, next)
}

fn not_impl(_: &mut Context, _: &[&term::Term], _: &mut [Var], _: &dyn Solver) -> Response {
    todo!()
}

type SolveFn =
    fn(ctx: &mut Context, args: &[&term::Term], substs: &mut [Var], next: &dyn Solver) -> Response;

static BUILTINS: phf::Map<&'static str, SolveFn> = phf_map! {
    "true/0" => not_impl,
    "fail/0" => not_impl,
    "call/1" => call::call,
    "!/0" => not_impl,
    ",/2" => not_impl,
    ";/2" => not_impl,
    "->/2" => not_impl,
    "catch/3" => not_impl,
    "throw/1" => not_impl,
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
    "\\+/1" => not_impl,
    "once/1" => not_impl,
    "repeat/0" => not_impl,
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
