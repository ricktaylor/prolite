use super::*;
use solve::{Frame, Solver};
use term::*;

pub(super) fn solve_write(frame: Frame, args: &[usize], next: &mut dyn Solver) -> Response {
    let term = if args.len() == 2 { &args[1] } else { &args[0] };
    print!("{:?}", *term);
    next.solve(frame)
}

pub(super) fn solve_nl(frame: Frame, _: &[usize], next: &mut dyn Solver) -> Response {
    println!();
    next.solve(frame)
}
