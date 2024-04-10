fn main() {
    let tp = turtle::parse_file("tmp.tg").unwrap();
    let mut itp = turtle::Interpreter::new(tp);
    itp.interpret();
}
