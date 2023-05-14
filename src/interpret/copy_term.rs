use super::*;
use term::*;

struct Substitutions<'a> {
    a: Vec<Var<'a>>,
    b: Vec<Var<'a>>,
    xref: HashMap<usize, &'a Rc<Term>>,
}

impl<'a> Substitutions<'a> {
    fn new(substs: &[Var<'a>]) -> Self {
        Self {
            a: substs.to_vec(),
            b: substs.to_vec(),
            xref: HashMap::new(),
        }
    }
}

fn unify_copy<'a>(
    a: &'a Rc<Term>,
    b: &'a Rc<Term>,
    mut substs: Substitutions<'a>,
) -> Result<Substitutions<'a>, Response> {
    match &a.kind {
        TermKind::Var(idx1) => {
            if let Some(a) = substs.a[*idx1] {
                unify_copy(a, b, substs)
            } else {
                match &b.kind {
                    TermKind::Var(idx2) => {
                        if let Some(b) = substs.b[*idx2] {
                            unify_copy(a, b, substs)
                        } else {
                            if let Some(&t) = substs.xref.get(idx1) {
                                substs.b[*idx2] = Some(t);
                            } else {
                                substs.xref.insert(*idx1, b);
                            }
                            Ok(substs)
                        }
                    }
                    _ => {
                        substs.a[*idx1] = Some(b);
                        Ok(substs)
                    }
                }
            }
        }
        TermKind::Integer(i1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_copy(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Integer(i2) if *i1 == *i2 => Ok(substs),
            TermKind::Float(f) if *i1 as f64 == *f => Ok(substs),
            _ => Err(Response::Fail),
        },
        TermKind::Float(f1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_copy(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Float(f2) if *f1 == *f2 => Ok(substs),
            TermKind::Integer(i) if *f1 == *i as f64 => Ok(substs),
            _ => Err(Response::Fail),
        },
        TermKind::Atom(s1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_copy(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Atom(s2) if *s1 == *s2 => Ok(substs),
            _ => Err(Response::Fail),
        },
        TermKind::Compound(c1) => match &b.kind {
            TermKind::Var(idx) => {
                if let Some(b) = substs.b[*idx] {
                    unify_copy(a, b, substs)
                } else {
                    substs.b[*idx] = Some(a);
                    Ok(substs)
                }
            }
            TermKind::Compound(c2)
                if c1.functor == c2.functor && c1.args.len() == c2.args.len() =>
            {
                c1.args
                    .iter()
                    .zip(&c2.args)
                    .try_fold(substs, |substs, (a, b)| unify_copy(a, b, substs))
            }
            _ => Err(Response::Fail),
        },
    }
}

pub(super) fn solve(
    ctx: &mut Context,
    args: &[Rc<Term>],
    substs: &[Var],
    next: &mut dyn Solver,
) -> Response {
    unify_copy(&args[0], &args[1], Substitutions::new(substs))
        .map_or_else(|r| r, |substs| next.solve(ctx, &substs.b))
}
