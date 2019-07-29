use super::ast::*;

pub fn print(node: AST<'_>) -> String {
    format!("{}", node)
}

#[test]
fn test_print_name() {
    assert_eq!(
        print(AST::Name(&Name {
            loc: None,
            value: "TestName".to_string(),
        })),
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
                    name: Name {
                        loc: None,
                        value: "QueryType".to_string(),
                    }
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
            name: Some(Name {
                loc: None,
                value: "MyQuery".to_string(),
            })
            .into(),
            variable_definitions: None.into(),
            directives: Some(vec![Directive {
                loc: None,
                arguments: None.into(),
                name: Name {
                    loc: None,
                    value: "dir".to_string(),
                }
            }])
            .into(),
            selection_set: SelectionSet {
                loc: None,
                selections: vec![Selection::Field(Box::new(Field {
                    loc: None,
                    alias: Some(Name {
                        loc: None,
                        value: "my_alias".to_string(),
                    })
                    .into(),
                    name: Name {
                        loc: None,
                        value: "random_field".to_string(),
                    },
                    arguments: Some(vec![
                        Argument {
                            loc: None,
                            name: Name {
                                loc: None,
                                value: "arg1".to_string(),
                            },
                            value: Value::StringValue(Box::new(StringValue {
                                loc: None,
                                value: "str".to_string(),
                                block: None,
                            }))
                        },
                        Argument {
                            loc: None,

                            name: Name {
                                loc: None,
                                value: "arg2".to_string(),
                            },
                            value: Value::Variable(Box::new(Variable {
                                loc: None,
                                name: Name {
                                    loc: None,
                                    value: "my_var".to_string(),
                                }
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