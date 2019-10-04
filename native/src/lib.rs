#[macro_use]
extern crate neon;
#[macro_use]
extern crate neon_serde;
#[macro_use]
extern crate serde_derive;

extern crate graphql_rs_native;

use std::fs;

struct JsVisitor {
    name: &'static Fn(&'static graphql_rs_native::language::ast::Name),
}

export! {
    fn parse(source: String) -> graphql_rs_native::language::ast::Document {
        source.into()
    }
    fn parse_file(source_file: String) -> () {
        let res: graphql_rs_native::language::ast::Document = fs::read_to_string(source_file).unwrap().into();
        assert_eq!(res.definitions.len(), 8);
    }
}

register_module!(mut m, {
    Ok(())
});