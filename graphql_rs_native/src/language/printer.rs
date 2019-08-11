use super::ast::*;

pub fn print(node: AST<'_>) -> String {
    format!("{}", node)
}

#[cfg(test)]
mod tests {
    use super::super::source::Source;
    use super::*;
    #[test]
    fn test_print_name() {
        assert_eq!(
            print(AST::Name(&Name::new(
                "TestName".to_string(),
                Location::new(0, 0, &Source::new("".to_owned(), None, None))
            ))),
            "TestName"
        )
    }

    #[test]
    fn test_print_schema_definition() {
        assert_eq!(
            print(AST::SchemaDefinition(&SchemaDefinition {
                loc: None,
                directives: None.into(),
                operation_types: vec![OperationTypeDefinition {
                    loc: None,
                    operation: OperationType::QUERY,
                    _type: NamedType {
                        loc: None,
                        name: Name::new(
                            "QueryType".to_string(),
                            Location::new(0, 0, &Source::new("".to_owned(), None, None))
                        )
                    }
                }]
                .into(),
            })),
            "schema {
  query: QueryType
}"
        )
    }

    #[test]
    fn test_print_operation_definition() {
        assert_eq!(
            print(AST::OperationDefinition(&OperationDefinition {
                loc: None,
                operation: OperationType::QUERY,
                name: Some(Name::new(
                    "MyQuery".to_string(),
                    Location::new(0, 0, &Source::new("".to_owned(), None, None))
                ))
                .into(),
                variable_definitions: None.into(),
                directives: Some(vec![Directive {
                    loc: None,
                    arguments: None.into(),
                    name: Name::new(
                        "dir".to_string(),
                        Location::new(0, 0, &Source::new("".to_owned(), None, None))
                    ),
                }])
                .into(),
                selection_set: SelectionSet {
                    loc: None,
                    selections: vec![Selection::Field(Box::new(Field {
                        loc: None,
                        alias: Some(Name::new(
                            "my_alias".to_string(),
                            Location::new(0, 0, &Source::new("".to_owned(), None, None))
                        ))
                        .into(),
                        name: Name::new(
                            "random_field".to_string(),
                            Location::new(0, 0, &Source::new("".to_owned(), None, None))
                        ),
                        arguments: Some(vec![
                            Argument {
                                loc: None,
                                name: Name::new(
                                    "arg1".to_string(),
                                    Location::new(0, 0, &Source::new("".to_owned(), None, None))
                                ),
                                value: Value::StringValue(Box::new(StringValue {
                                    loc: None,
                                    value: "str".to_string(),
                                    block: None,
                                }))
                            },
                            Argument {
                                loc: None,
                                name: Name::new(
                                    "arg2".to_string(),
                                    Location::new(0, 0, &Source::new("".to_owned(), None, None))
                                ),
                                value: Value::Variable(Box::new(Variable {
                                    loc: None,
                                    name: Name::new(
                                        "my_var".to_string(),
                                        Location::new(
                                            0,
                                            0,
                                            &Source::new("".to_owned(), None, None)
                                        )
                                    )
                                }))
                            }
                        ])
                        .into(),
                        directives: None.into(),
                        selection_set: None,
                    }))]
                }
            })),
            "query MyQuery @dir {
  my_alias: random_field(arg1: \"str\", arg2: $my_var)
}"
        )
    }
}
