use std::collections::HashSet;

use super::*;
use frame::Frame;
use solve::{Continuation, Solver};
use term::TermKind;

fn copy_out(frame: &Frame, term: usize) -> Rc<read_term::Term> {
    copy_out_inner(frame, term, &mut HashMap::new())
}

fn copy_out_inner(
    frame: &Frame,
    term: usize,
    var_index: &mut HashMap<usize, Rc<read_term::Term>>,
) -> Rc<read_term::Term> {
    let (term, _) = frame.get_term(term);
    match (&term.kind, &term.source.kind) {
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            read_term::Term::new_compound(
                c.functor.clone(),
                term.source.location.clone(),
                args.iter()
                    .map(|a| copy_out_inner(frame, *a, var_index))
                    .collect(),
            )
        }
        (TermKind::Var(idx), _) => {
            if let Some(i) = var_index.get(idx) {
                i.clone()
            } else {
                let term = Rc::new(read_term::Term {
                    kind: read_term::TermKind::Var(*idx),
                    location: term.source.location.clone(),
                });
                var_index.insert(*idx, term.clone());
                term
            }
        }
        _ => term.source.clone(),
    }
}

fn findall_var(
    mut frame: Frame,
    template: usize,
    goal: usize,
    instances: usize,
    next: &mut dyn Solver,
) -> Response {
    let mut solutions = Vec::new();
    frame
        .sub_frame(|frame| {
            solve::call(
                frame,
                goal,
                &mut Continuation::new(|frame| {
                    solutions.push(copy_out(&frame, template));
                    Response::Fail
                }),
            )
        })
        .map_failed(|| {
            frame.sub_frame(|mut frame| {
                let solutions: Vec<usize> = solutions.iter().map(|t| frame.new_term(t)).collect();
                let list = frame.list_from_slice(&solutions);
                if frame.unify(list, instances) {
                    next.solve(frame)
                } else {
                    Response::Fail
                }
            })
        })
}

fn findall_list(
    mut frame: Frame,
    template: usize,
    goal: usize,
    mut head: usize,
    mut tail: usize,
    next: &mut dyn Solver,
) -> Response {
    let mut solutions = Vec::new();
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            goal,
            &mut Continuation::new(|mut frame| {
                let (term, h2) = frame.get_term(head);
                head = h2;

                match (&term.kind, &term.source.kind) {
                    (TermKind::Var(_), _) => {
                        solutions.push(copy_out(&frame, template));
                        return Response::Fail;
                    }
                    (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => {
                        // No more matches expected!
                        return Response::Cut;
                    }
                    _ => {}
                }

                let t = frame.copy_term(template);
                if !frame.unify(t, head) {
                    return Response::Cut;
                }

                let (term, t2) = frame.get_term(tail);
                match (&term.kind, &term.source.kind) {
                    (TermKind::Compound(args), read_term::TermKind::Compound(c))
                        if c.functor == "." && args.len() == 2 =>
                    {
                        head = args[0];
                        tail = args[1];
                    }
                    _ => {
                        tail = t2;
                        head = tail
                    }
                }
                Response::Fail
            }),
        )
    }) {
        Response::Fail => frame.sub_frame(|mut frame| {
            let solutions: Vec<usize> = solutions.iter().map(|t| frame.new_term(t)).collect();
            let list = frame.list_from_slice(&solutions);
            if frame.unify(list, tail) {
                next.solve(frame)
            } else {
                Response::Fail
            }
        }),
        Response::Cut => Response::Fail,
        r => r,
    }
}

fn findall_none(mut frame: Frame, goal: usize, next: &mut dyn Solver) -> Response {
    match frame.sub_frame(|frame| {
        solve::call(
            frame,
            goal,
            &mut Continuation::new(|_| {
                // No solutions allowed
                Response::Cut
            }),
        )
    }) {
        Response::Fail => next.solve(frame),
        Response::Cut => Response::Fail,
        r => r,
    }
}

pub fn findall(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let (instances, _) = frame.get_term(args[2]);
    match (&instances.kind, &instances.source.kind) {
        (TermKind::Var(_), _) => findall_var(frame, args[0], args[1], args[2], next),
        (TermKind::Compound(c_args), read_term::TermKind::Compound(c))
            if c.functor == "." && c_args.len() == 2 =>
        {
            let head = c_args[0];
            let tail = c_args[1];
            findall_list(frame, args[0], args[1], head, tail, next)
        }
        (TermKind::Atomic, read_term::TermKind::Atom(s)) if s == "[]" => {
            findall_none(frame, args[1], next)
        }
        _ => {
            // Error
            todo!()
        }
    }
}

fn add_vars(frame: &Frame, t: usize, vars: &mut HashSet<usize>) {
    let (term, _) = frame.get_term(t);
    match &term.kind {
        TermKind::Var(idx) => {
            vars.insert(*idx);
        }
        TermKind::Compound(args) => {
            for a in args.iter() {
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
    let (term, _) = frame.get_term(t);
    match &term.kind {
        TermKind::Var(idx) => {
            if other_vars.get(idx).is_none() {
                free_vars.insert(*idx);
            }
        }
        TermKind::Compound(args) => {
            for a in args.iter() {
                find_free_vars(frame, *a, free_vars, other_vars);
            }
        }
        _ => {}
    }
}

fn existential_split(frame: &Frame, t: usize, other_vars: &mut HashSet<usize>) -> usize {
    let (term, t) = frame.get_term(t);
    match (&term.kind, &term.source.kind) {
        (TermKind::Compound(args), read_term::TermKind::Compound(c))
            if c.functor == "^" && args.len() == 2 =>
        {
            add_vars(frame, args[0], other_vars);
            existential_split(frame, args[1], other_vars)
        }
        _ => t,
    }
}

fn split_free_vars(frame: &Frame, t: usize, v: usize, free_vars: &mut HashSet<usize>) -> usize {
    let mut other_vars = HashSet::new();
    add_vars(frame, v, &mut other_vars);
    let goal = existential_split(frame, t, &mut other_vars);
    find_free_vars(frame, goal, free_vars, &other_vars);
    goal
}

fn setof_empty(
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
                &mut Continuation::new(|frame| {
                    solutions.push(copy_out(&frame, witness));
                    Response::Fail
                }),
            )
        })
        .map_failed(|| {
            if solutions.is_empty() {
                Response::Fail
            } else {
                frame.sub_frame(|mut frame| {
                    let solutions: Vec<usize> =
                        solutions.iter().map(|t| frame.new_term(t)).collect();
                    let list = frame.list_from_slice(&solutions);
                    if frame.unify(list, instances) {
                        next.solve(frame)
                    } else {
                        Response::Fail
                    }
                })
            }
        })
}

fn setof_freevars(
    _frame: Frame,
    _witness: usize,
    _goal: usize,
    _free_vars: HashSet<usize>,
    _instances: usize,
    _next: &mut dyn Solver,
) -> Response {
    todo!()
}

pub fn setof(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    // Find the free variables of the iterated goal of arg[1] wrt arg[0]
    let mut free_vars = HashSet::new();
    let goal = split_free_vars(&frame, args[1], args[0], &mut free_vars);

    if free_vars.is_empty() {
        setof_empty(frame, args[0], goal, args[2], next)
    } else {
        setof_freevars(frame, args[0], goal, free_vars, args[2], next)
    }
}
