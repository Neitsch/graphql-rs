#[macro_use]
extern crate neon;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate neon_serde;
#[macro_use]
extern crate serde_derive;

mod language;

use neon::prelude::*;

export! {
    fn parse(source: String) -> language::ast::Document {
        source.into()
    }
}
