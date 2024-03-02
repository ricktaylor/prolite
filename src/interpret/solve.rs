use super::*;
use frame::Frame;
use term::TermKind;

pub type SolveFn = fn(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response;

pub trait Solver {
    fn solve(&mut self, frame: Frame) -> Response;
}

pub struct Continuation<F>
where
    F: FnMut(Frame) -> Response,
{
    solve: F,
}

impl<F> Continuation<F>
where
    F: FnMut(Frame) -> Response,
{
    pub fn new(f: F) -> Self {
        Self { solve: f }
    }
}

impl<F> Solver for Continuation<F>
where
    F: FnMut(Frame) -> Response,
{
    fn solve(&mut self, frame: Frame) -> Response {
        (self.solve)(frame)
    }
}

pub trait Control {
    fn exec(&mut self, frame: Frame, next: &mut dyn Solver) -> Response;
}

pub type Goal = Box<dyn Control>;

pub type GenerateFn = fn(frame: &Frame, args: &[usize]) -> Option<Goal>;
pub type CompileFn = fn(args: &[Rc<read_term::Term>]) -> Option<Goal>;

struct True {}

impl Control for True {
    fn exec(&mut self, frame: Frame, next: &mut dyn Solver) -> Response {
        next.solve(frame)
    }
}

pub fn generate_true(_: &Frame, _: &[usize]) -> Option<Goal> {
    Some(Box::new(True {}))
}

pub fn compile_true(_: &[Rc<read_term::Term>]) -> Option<Goal> {
    Some(Box::new(True {}))
}

struct Fail {}

impl Control for Fail {
    fn exec(&mut self, _: Frame, _: &mut dyn Solver) -> Response {
        Response::Fail
    }
}

pub fn generate_fail(_: &Frame, _: &[usize]) -> Option<Goal> {
    Some(Box::new(Fail {}))
}

pub fn compile_fail(_: &[Rc<read_term::Term>]) -> Option<Goal> {
    Some(Box::new(Fail {}))
}

struct Cut {}

impl Control for Cut {
    fn exec(&mut self, frame: Frame, next: &mut dyn Solver) -> Response {
        next.solve(frame).map_failed(|| Response::Cut)
    }
}

pub fn generate_cut(_: &Frame, _: &[usize]) -> Option<Goal> {
    Some(Box::new(Cut {}))
}

pub fn compile_cut(_: &[Rc<read_term::Term>]) -> Option<Goal> {
    Some(Box::new(Cut {}))
}

struct GoalSet {
    goals: Vec<Goal>,
}

impl Control for GoalSet {
    fn exec(&mut self, frame: Frame, mut next: &mut dyn Solver) -> Response {
        self.goals
            .iter()
            .rev()
            .fold(next, |next, g| {
                &mut Continuation::new(|frame| g.exec(frame, next))
            })
            .solve(frame)
    }
}

fn new_head_goal(args: Vec<Goal>) -> Option<Goal> {
    Some(Box::new(GoalSet { goals: args }))
}

struct And {
    first: Goal,
    second: Goal,
}

impl Control for And {
    fn exec(&mut self, frame: Frame, next: &mut dyn Solver) -> Response {
        self.first.exec(
            frame,
            &mut Continuation::new(|frame| self.second.exec(frame, next)),
        )
    }
}

pub fn generate_and(frame: &Frame, args: &[usize]) -> Option<Goal> {
    Some(Box::new(And {
        first: match generate(frame, args[0]) {
            None => return None,
            Some(g) => g,
        },
        second: match generate(frame, args[1]) {
            None => return None,
            Some(g) => g,
        },
    }))
}

pub fn compile_and(args: &[Rc<read_term::Term>]) -> Option<Goal> {
    Some(Box::new(And {
        first: match compile(&args[0]) {
            None => return None,
            Some(g) => g,
        },
        second: match compile(&args[1]) {
            None => return None,
            Some(g) => g,
        },
    }))
}

struct Or {
    either: Goal,
    or: Goal,
}

impl Control for Or {
    fn exec(&mut self, mut frame: Frame, next: &mut dyn Solver) -> Response {
        frame
            .sub_frame(|frame| self.either.exec(frame, next))
            .map_failed(|| self.or.exec(frame, next))
    }
}

pub fn generate_or(frame: &Frame, args: &[usize]) -> Option<Goal> {
    let (either, _) = frame.get_term(args[0]);
    match (&either.kind, &either.source.kind) {
        (TermKind::Compound(c_args), read_term::TermKind::Compound(c))
            if c.functor == "->" && c_args.len() == 2 =>
        {
            generate_if_then_else(frame, c_args[0], c_args[1], Some(args[1]))
        }
        _ => Some(Box::new(Or {
            either: match generate(frame, args[0]) {
                None => return None,
                Some(g) => g,
            },
            or: match generate(frame, args[1]) {
                None => return None,
                Some(g) => g,
            },
        })),
    }
}

pub fn compile_or(args: &[Rc<read_term::Term>]) -> Option<Goal> {
    match &args[0].kind {
        read_term::TermKind::Compound(c) if c.functor == "->" && c.args.len() == 2 => {
            compile_if_then_else(&c.args[0], &c.args[1], Some(&args[1]))
        }
        _ => Some(Box::new(Or {
            either: match compile(&args[0]) {
                None => return None,
                Some(g) => g,
            },
            or: match compile(&args[1]) {
                None => return None,
                Some(g) => g,
            },
        })),
    }
}

struct IfThenElse {
    if_goal: Goal,
    then_goal: Goal,
    else_goal: Option<Goal>,
}

impl Control for IfThenElse {
    fn exec(&mut self, mut frame: Frame, next: &mut dyn Solver) -> Response {
        let mut if_true = false;
        let mut then_cut = false;
        frame
            .sub_frame(|frame| {
                self.if_goal.exec(
                    frame,
                    &mut Continuation::new(|frame| {
                        if_true = true;
                        self.then_goal
                            .exec(frame, next)
                            .map_cut(|| {
                                then_cut = true;
                                Response::Cut
                            })
                            .map_failed(|| Response::Cut)
                    }),
                )
            })
            .map_failed(|| match &mut self.else_goal {
                Some(else_goal) if !if_true => else_goal.exec(frame, next),
                _ => Response::Fail,
            })
    }
}

pub fn generate_if_then(frame: &Frame, args: &[usize]) -> Option<Goal> {
    generate_if_then_else(frame, args[0], args[1], None)
}

pub fn compile_if_then(args: &[Rc<read_term::Term>]) -> Option<Goal> {
    compile_if_then_else(&args[0], &args[1], None)
}

fn generate_if_then_else(
    frame: &Frame,
    if_term: usize,
    then_term: usize,
    else_term: Option<usize>,
) -> Option<Goal> {
    Some(Box::new(IfThenElse {
        if_goal: match generate(frame, if_term) {
            None => return None,
            Some(g) => g,
        },
        then_goal: match generate(frame, then_term) {
            None => return None,
            Some(g) => g,
        },
        else_goal: match else_term {
            None => None,
            Some(else_term) => match generate(frame, else_term) {
                None => return None,
                g => g,
            },
        },
    }))
}

fn compile_if_then_else(
    if_term: &Rc<read_term::Term>,
    then_term: &Rc<read_term::Term>,
    else_term: Option<&Rc<read_term::Term>>,
) -> Option<Goal> {
    Some(Box::new(IfThenElse {
        if_goal: match compile(if_term) {
            None => return None,
            Some(g) => g,
        },
        then_goal: match compile(then_term) {
            None => return None,
            Some(g) => g,
        },
        else_goal: match else_term {
            None => None,
            Some(else_term) => match compile(else_term) {
                None => return None,
                g => g,
            },
        },
    }))
}

struct Call {
    next: Goal,
}

impl Control for Call {
    fn exec(&mut self, frame: Frame, next: &mut dyn Solver) -> Response {
        let mut next_cut = false;
        self.next
            .exec(
                frame,
                &mut Continuation::new(|frame| {
                    next.solve(frame).map_cut(|| {
                        next_cut = true;
                        Response::Cut
                    })
                }),
            )
            .map_cut(|| {
                // call/1 is "not transparent" to cut
                if !next_cut {
                    Response::Fail
                } else {
                    Response::Cut
                }
            })
    }
}

fn generate_call(frame: &Frame, args: &[usize]) -> Option<Goal> {
    Some(Box::new(Call {
        next: match generate(frame, args[0]) {
            None => return None,
            Some(g) => g,
        },
    }))
}

pub fn call(frame: Frame, term: usize, next: &mut dyn Solver) -> Response {
    match generate_call(&frame, &[term]) {
        None => {
            let (term, _) = frame.get_term(term);
            return throw::type_error("callable", &term.source);
        }
        Some(g) => g,
    }
    .exec(frame, next)
}

struct Var {
    term: usize,
}

impl Control for Var {
    fn exec(&mut self, frame: Frame, next: &mut dyn Solver) -> Response {
        let (term, t) = frame.get_term(self.term);
        if let TermKind::Var(_) = &term.kind {
            throw::instantiation_error(&term.source)
        } else {
            call(frame, t, next)
        }
    }
}

pub fn generate(frame: &Frame, term: usize) -> Option<Goal> {
    let (term, t) = frame.get_term(term);
    match (&term.kind, &term.source.kind) {
        (TermKind::Atomic, read_term::TermKind::Atom(s)) => {
            let pi = format!("{}/0", s);
            match builtins::get_builtin(&pi) {
                Some(builtins::Builtin::Solve(f)) => builtins::generate(&pi, *f, &[]),
                Some(builtins::Builtin::Control(_, generate)) => (generate)(frame, &[]),
                None => user_defined::generate(pi, t),
            }
        }
        (TermKind::Var(_), _) => Some(Box::new(Var { term: t })),
        (TermKind::Compound(args), read_term::TermKind::Compound(c)) => {
            let pi = format!("{}/{}", c.functor, args.len());
            match builtins::get_builtin(&pi) {
                Some(builtins::Builtin::Solve(f)) => builtins::generate(&pi, *f, args),
                Some(builtins::Builtin::Control(_, generate)) => (generate)(frame, args),
                None => user_defined::generate(pi, t),
            }
        }
        _ => None,
    }
}

pub fn compile(term: &Rc<read_term::Term>) -> Option<Goal> {
    match &term.kind {
        read_term::TermKind::Atom(s) => {
            let pi = format!("{}/0", s);
            match builtins::get_builtin(&pi) {
                Some(builtins::Builtin::Solve(f)) => builtins::compile(&pi, *f, &[]),
                Some(builtins::Builtin::Control(compile, _)) => (compile)(&[]),
                None => user_defined::compile(pi, &[]),
            }
        }
        read_term::TermKind::Var(idx) => Some(Box::new(Var { term: t })),
        read_term::TermKind::Compound(c) => {
            let pi = format!("{}/{}", c.functor, c.args.len());
            match builtins::get_builtin(&pi) {
                Some(builtins::Builtin::Solve(f)) => builtins::compile(&pi, *f, &c.args),
                Some(builtins::Builtin::Control(compile, _)) => (compile)(&c.args),
                None => user_defined::compile(pi, &c.args),
            }
        }
        _ => None,
    }
}

// Possibly test only!
pub fn eval<F: FnMut() -> bool>(
    ctx: &mut Context,
    goal: &Rc<read_term::Term>,
    mut callback: F,
) -> Response {
    let mut cache = Vec::new();
    let mut substs = Vec::new();
    let mut frame = Frame::new(ctx, &mut cache, &mut substs);
    let goal = frame.new_term(goal);
    call(
        frame,
        goal,
        &mut Continuation::new(|_| {
            if callback() {
                Response::Fail
            } else {
                Response::Cut
            }
        }),
    )
}
