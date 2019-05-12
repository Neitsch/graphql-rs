#[macro_use]
extern crate neon;
#[macro_use]
extern crate neon_serde;

extern crate graphql_rs_native;

use std::fs;

export! {
    fn parse(source: String) -> graphql_rs_native::language::ast::Document {
        source.into()
    }
    fn parse_file(source_file: String) -> () {
        let res: graphql_rs_native::language::ast::Document = fs::read_to_string(source_file).unwrap().into();
        assert_eq!(res.definitions.len(), 8);
    }
}
