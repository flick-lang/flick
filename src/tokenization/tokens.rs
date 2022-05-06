use super::Token;

#[derive(Debug)]
pub struct Tokens {

}

impl Iterator for Tokens {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
