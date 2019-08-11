/// This is a struct that represents the cursor location in the source document. Note that it uses line and column as opposed to a raw offset.
#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    line: u64,
    column: u64,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location {
            line: line as u64,
            column: column as u64,
        }
    }
}

/// A representation of source input to GraphQL.
/// `name` and `locationOffset` are optional. They are useful for clients who
/// store GraphQL documents in source files; for example, if the GraphQL input
/// starts at line 40 in a file named Foo.graphql, it might be useful for name to
/// be "Foo.graphql" and location to be `{ line: 40, column: 1 }`.
/// line and column in locationOffset are 1-indexed
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Source {
    /// The graphql source text
    #[serde(skip_serializing)]
    pub body: String,
    /// The name of the GraphQL document if applicable
    name: String,
    #[serde(rename = "locationOffset")]
    location_offset: Location,
}

impl Source {
    /// Creates a new Source object. The most important value is the `body`, which is the graphql source text
    /// ```
    /// # use graphql_rs_native::language::source::Source;
    /// let source_text = "type User { id: ID }".to_string();
    /// let source = Source::new(source_text.clone(), None, None);
    /// assert_eq!(source.body, source_text)
    /// ```
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
