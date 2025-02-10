use crate::core::errors::FormattingError;

pub trait LineParsing {
    fn into_line(self) -> String where Self: Sized{
        self.combine_line()
    }
    fn combine_line(&self) -> String;
    fn parse_from_line(line: &str) -> Result<Self, FormattingError> where Self: Sized;
}