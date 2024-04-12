fn main() {
    let (tp, idents) = turtle::parse_file("examples/pyth_frak.tg").unwrap();
    //let mut itp = turtle::Interpreter::new(tp);
    //itp.interpret();
    let cc = turtle::CComp::new(tp, &idents);
    cc.compile();
}
