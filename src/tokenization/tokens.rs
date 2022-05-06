use super::Token;

#[derive(Debug)]
pub struct Tokens<'a> {

}

impl<'a> Iterator for Tokens<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
