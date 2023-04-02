use super::*;

#[test]
fn test_read_term() {

    test_term("test_false((1 \\= 1)).");
    test_term("test_false((\\+ (true))).");
    test_term("defined((\\+)/1).");

}

fn test_term(s: &str)
{
    let ctx: Context = Default::default();
    let mut stream = utf8reader::Utf8Reader::new(s.as_bytes(),s);

    let r = parser::next(&ctx,&mut stream, false);
    if let Err(e) = r {
        println!("{:?}", e);
    }
}
