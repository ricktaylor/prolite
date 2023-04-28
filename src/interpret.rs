mod builtins;
mod call;

use super::read_term::*;

pub(crate) enum Response {
    False,
    Cut,
    Throw,
    Halt(isize),
}

pub(crate) struct Var<'a> {
    pub name: String,
    pub value: Option<&'a term::Term>,
}

pub(super) struct Context {}

impl Default for Context {
    fn default() -> Self {
        Self {}
    }
}

trait Solver {
    fn call(
        &self,
        ctx: &mut Context,
        args: &[&term::Term],
        substs: &mut [Var],
        next: &dyn Solver,
    ) -> Response;
}

struct CallbackSolver<F: Fn(&mut [Var]) -> Response> {
    callback: F,
}

impl<F> Solver for CallbackSolver<F>
where
    F: Fn(&mut [Var]) -> Response,
{
    fn call(
        &self,
        _: &mut Context,
        _: &[&term::Term],
        substs: &mut [Var],
        _: &dyn Solver,
    ) -> Response {
        (self.callback)(substs)
    }
}

pub(crate) fn eval<F: Fn(&mut [Var]) -> Response>(goal: term::Term, callback: F) -> Response {
    let mut substs = Vec::new();

    call::call(
        &mut Context::default(),
        &[&goal],
        &mut substs,
        &CallbackSolver { callback },
    )
}
