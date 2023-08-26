use jlang_parse::token::*;

fn main() {
    let input = std::fs::read_to_string("./jlang-test.jlang").unwrap();

    for result in stream::TokenStream::new(&input) {
        match result {
            Ok(Token { kind, .. }) => println!("{kind}"),
            Err(err) => println!("{err}"),
        }
    }
}
