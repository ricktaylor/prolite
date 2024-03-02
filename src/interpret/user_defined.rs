use super::*;
use frame::Frame;
use solve::{Continuation, Control, Goal, Solver};
use term::TermKind;

#[derive(Debug)]
pub struct Clause {
    //head: Rc<read_term::Term>,
    //body: Option<Rc<Term>>,
    source: Option<consult::text::Clause>,
}

#[derive(Debug)]
pub struct Procedure {
    pub dynamic: bool,
    pub predicates: Vec<Rc<Clause>>,
}

fn unpack_pi(frame: &Frame, term: usize) -> Result<(String, consult::text::Clause), Response> {
    let (term, _) = frame.get_term(term);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Atom(s)) => Ok((
            format!("{}/0", s),
            consult::text::Clause {
                head: term.source.clone(),
                body: None,
            },
        )),
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            if c.functor == ":-" && args.len() == 2 {
                let (head, _) = frame.get_term(args[0]);
                let functor = match (&head.kind, &head.source.kind) {
                    (TermKind::Atomic, read_term::TermKind::Atom(s)) => format!("{}/0", s),
                    (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
                        format!("{}/{}", c.functor, args.len())
                    }
                    (TermKind::Var(_), _) => return Err(throw::instantiation_error(&head.source)),
                    _ => return Err(throw::callable(&head.source)),
                };
                let (body, _) = frame.get_term(args[1]);
                Ok((
                    functor,
                    consult::text::Clause {
                        head: head.source.clone(),
                        body: Some(body.source.clone()),
                    },
                ))
            } else {
                Ok((
                    format!("{}/{}", c.functor, args.len()),
                    consult::text::Clause {
                        head: term.source.clone(),
                        body: None,
                    },
                ))
            }
        }
        (TermKind::Var(_), _) => Err(throw::instantiation_error(&term.source)),
        _ => Err(throw::callable(&term.source)),
    }
}

fn permission_error(culprit: &Rc<read_term::Term>) -> Response {
    throw::error(
        read_term::Term::new_compound(
            "permission_error".to_string(),
            None,
            vec![
                read_term::Term::new_atom("modify".to_string(), None),
                read_term::Term::new_atom("static_procedure".to_string(), None),
                culprit.as_pi().unwrap(),
            ],
        ),
        culprit.location.clone(),
    )
}

pub fn assert(frame: Frame, goal: usize, is_z: bool, next: &mut dyn Solver) -> Response {
    unpack_pi(&frame, goal).map_or_else(
        |r| r,
        |(pi, clause)| {
            if builtins::is_builtin(&pi) {
                return permission_error(&clause.head);
            }

            //let body = body.map_or(None, |body| generate(&frame, body));

            let procedures = &mut frame.context.procedures;
            if let Some(p) = procedures.get_mut(&pi) {
                if !p.dynamic {
                    return permission_error(&clause.head);
                }

                let clause = Rc::new(Clause {
                    source: Some(clause),
                });

                if is_z {
                    p.predicates.push(clause);
                } else {
                    p.predicates.insert(0, clause);
                }
            } else {
                procedures.insert(
                    pi.clone(),
                    Procedure {
                        predicates: vec![Rc::new(Clause {
                            source: Some(clause),
                        })],
                        dynamic: true,
                    },
                );
            }
            next.solve(frame)
        },
    )
}

pub fn import(
    ctx: &mut Context,
    pi: String,
    procedure: consult::text::Procedure,
) -> Result<(), Response> {
    ctx.procedures.insert(
        pi,
        Procedure {
            dynamic: procedure.dynamic,
            predicates: procedure
                .predicates
                .into_iter()
                .map(|clause| {
                    Rc::new(Clause {
                        source: procedure.dynamic.then_some(clause),
                    })
                })
                .collect(),
        },
    );
    Ok(())
}

struct GoalJit {
    pi: String,
    term: usize,
}

impl Control for GoalJit {
    fn exec(&mut self, mut frame: Frame, next: &mut dyn Solver) -> Response {
        let predicates = match frame.context.procedures.get(&self.pi) {
            None => {
                return match frame.context.flags.unknown {
                    crate::flags::UnknownFlag::Error => {
                        let (term, _) = frame.get_term(self.term);
                        throw::callable(&term.source)
                    }
                    //crate::flags::UnknownFlag::Warning => todo!(), // Some kind of warning?
                    _ => Response::Fail,
                };
            }
            Some(p) => p.predicates.to_vec(),
        };
        for clause in predicates {
            match frame.sub_frame(|mut frame| {
                let mut index = HashMap::new();
                let head = frame.new_term_indexed(&clause.head, &mut index);
                if frame.unify(self.term, head) {
                    if let Some(body) = &clause.body {
                        let b = frame.new_term_indexed(body, &mut index);
                        match solve::generate(&frame, b) {
                            None => {
                                let (term, _) = frame.get_term(b);
                                return throw::type_error("callable", &term.source);
                            }
                            Some(g) => g,
                        }
                        .exec(frame, next)
                    } else {
                        next.solve(frame)
                    }
                } else {
                    Response::Fail
                }
            }) {
                Response::Fail => {}
                r => return r,
            }
        }
        Response::Fail
    }
}

pub fn generate(pi: String, term: usize) -> Option<Goal> {
    Some(Box::new(GoalJit { pi, term }))
}

struct GoalCompiled {}

impl Control for GoalCompiled {
    fn exec(&mut self, mut frame: Frame, next: &mut dyn Solver) -> Response {
        todo!()
    }
}

pub fn compile(pi: String, args: &[Rc<read_term::Term>]) -> Option<Goal> {
    Some(Box::new(GoalCompiled {}))
}
