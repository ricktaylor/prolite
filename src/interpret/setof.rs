use std::collections::HashSet;

use super::*;
use solve::{Continuation, Frame, Solver};
use term::*;

fn add_vars(frame: &Frame, t: usize, vars: &mut HashSet<usize>) {
    match frame.get_term(t) {
        Term::Var(idx) => {
            if let Some(t) = frame.get_var(*idx) {
                add_vars(frame, t, vars)
            } else {
                vars.insert(*idx);
            }
        }
        Term::Compound(c) => {
            for a in c.args.iter() {
                add_vars(frame, *a, vars);
            }
        }
        _ => {}
    }
}

fn find_free_vars(
    frame: &Frame,
    t: usize,
    free_vars: &mut HashSet<usize>,
    other_vars: &HashSet<usize>,
) {
    match frame.get_term(t) {
        Term::Var(idx) => {
            if let Some(t) = frame.get_var(*idx) {
                find_free_vars(frame, t, free_vars, other_vars);
            } else if other_vars.get(idx).is_none() {
                free_vars.insert(*idx);
            }
        }
        Term::Compound(c) => {
            for a in c.args.iter() {
                find_free_vars(frame, *a, free_vars, other_vars);
            }
        }
        _ => {}
    }
}

fn existential_split(frame: &Frame, t: usize, other_vars: &mut HashSet<usize>) -> usize {
    match frame.get_term(t) {
        Term::Compound(c) if c.functor() == "^" && c.args.len() == 2 => {
            add_vars(frame, c.args[0], other_vars);
            existential_split(frame, c.args[1], other_vars)
        }
        Term::Var(idx) => {
            if let Some(t) = frame.get_var(*idx) {
                existential_split(frame, t, other_vars)
            } else {
                t
            }
        }
        _ => t,
    }
}

fn split_free_vars(frame: &Frame, t: usize, v: usize, free_vars: &mut HashSet<usize>) -> usize {
    let mut other_vars = HashSet::new();
    add_vars(frame, v, &mut other_vars);

    eprintln!("template vars: {:?}", &other_vars);

    let goal = existential_split(frame, t, &mut other_vars);

    eprintln!("with existential vars: {:?}", &other_vars);

    find_free_vars(frame, goal, free_vars, &other_vars);
    goal
}

fn solve_empty(
    mut frame: Frame,
    witness: usize,
    goal: usize,
    instances: usize,
    next: &mut dyn Solver,
) -> Response {
    /* We cannot short-cut like we do with findall/3 as the
     * standard specifies that findall(W,Goal,S) is run before unification with Instances
     * This allows for Goal with side-effects
     */
    let mut solutions = Vec::new();
    frame
        .sub_frame(|frame| {
            solve::call(
                frame,
                goal,
                &mut Continuation::new(|mut frame| {
                    solutions.push(frame.copy_term(witness));
                    Response::Fail
                }),
            )
        })
        .map_failed(|| {
            if solutions.is_empty() {
                Response::Fail
            } else {
                let list = frame.as_list(&solutions);
                solve::unify(frame, list, instances, next)
            }
        })
}

fn solve_freevars(
    _frame: Frame,
    _witness: usize,
    _goal: usize,
    _free_vars: HashSet<usize>,
    _instances: usize,
    _next: &mut dyn Solver,
) -> Response {
    todo!()
}

pub(super) fn solve(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    // Find the free variables of the iterated goal of arg[1] wrt arg[0]
    let mut free_vars = HashSet::new();
    let goal = split_free_vars(&frame, args[1], args[0], &mut free_vars);

    eprintln!("free vars: {:?}", &free_vars);

    if free_vars.is_empty() {
        solve_empty(frame, args[0], goal, args[2], next)
    } else {
        solve_freevars(frame, args[0], goal, free_vars, args[2], next)
    }
}
