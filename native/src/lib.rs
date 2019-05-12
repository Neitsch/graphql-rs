#[macro_use]
extern crate neon;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate neon_serde;
#[macro_use]
extern crate serde_derive;

mod language;

use std::fs;

export! {
    fn parse(source: String) -> language::ast::Document {
        source.into()
    }
    fn parse_file(source_file: String) -> () {
        let res: language::ast::Document = fs::read_to_string(source_file).unwrap().into();
        assert_eq!(res.definitions.len(), 8);
    }
}
