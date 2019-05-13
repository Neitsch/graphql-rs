#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    line: u64,
    column: u64,
}

/// A representation of source input to GraphQL.
/// `name` and `locationOffset` are optional. They are useful for clients who
/// store GraphQL documents in source files; for example, if the GraphQL input
/// starts at line 40 in a file named Foo.graphql, it might be useful for name to
/// be "Foo.graphql" and location to be `{ line: 40, column: 1 }`.
/// line and column in locationOffset are 1-indexed
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Source {
    pub body: String,
    name: String,
    #[serde(rename = "locationOffset")]
    location_offset: Location,
}

impl Source {
    pub fn new(body: String, name: Option<String>, location_offset: Option<Location>) -> Source {
        Source {
            body,
            name: (match name {
                Some(v) => v,
                None => "GraphQL Request".to_string(),
            }),
            location_offset: (match location_offset {
                Some(v) => {
                    assert!(v.line > 0);
                    assert!(v.column > 0);
                    v
                }
                None => Location { line: 1, column: 1 },
            }),
        }
    }
}
