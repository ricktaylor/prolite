use super::*;

pub fn read_term(s: &str) {
    Context {
        flags: &flags::Flags::default(),
        operators: &operators::Operator::default_table(),
        char_conversion: &HashMap::new(),
        greedy: false,
    }
    .read_term(s)
    .unwrap()
    .unwrap();
}

#[test]
fn test() {
    read_term("test_false((1 \\= 1)).");
    read_term("test_false((\\+ (true))).");
    read_term("defined((\\+)/1).");
    read_term("/(7,35).");
    read_term("X21 is /(7,35).");
    read_term("0.2000.");
    read_term("test_val(X21 is /(7,35),X21,0.2000).");
}
