use nom::types::CompleteByteSlice;
use nom::{
    alt, call, delimited, do_parse, many0, many1, map, named, named_args, one_of, opt, pair, peek,
    preceded, recognize, separated_list, separated_pair, switch, tag, take_until, take_while,
    take_while1, terminated,
};

use super::ast::*;
use super::source::Source;

fn whitespace(c: u8) -> bool {
    (c == b' ') || (c == b'\t') || (c == b'\n') || (c == b',')
}

named!(
    whitespace0<CompleteByteSlice<'_>, CompleteByteSlice<'_>>,
    recognize!(many0!(alt!(
        do_parse!(
            tag!("#")
            >> take_until!("\n")
            >> (())
        ) => {|_| ()} |
        take_while!(whitespace) => {|_| ()}
    )))
);

#[test]
fn test_whitespace() {
    assert_eq!(
        whitespace0(CompleteByteSlice(b" # test\n")),
        Ok((CompleteByteSlice(b""), CompleteByteSlice(b" # test\n")))
    );
}

fn is_alphanumeric_or_underscore(c: u8) -> bool {
    nom::is_alphanumeric(c) || (c == b'_')
}

named_args!(
    punct<'a>(tag_value: &str) <CompleteByteSlice<'a>, CompleteByteSlice<'a>>,
    do_parse!(
        whitespace0
        >> t: tag!(tag_value)
        >> whitespace0
        >> (t)
    )
);

named!(
    name<CompleteByteSlice<'_>, Name>,
    map!(
        delimited!(
            whitespace0,
            take_while1!(is_alphanumeric_or_underscore),
            whitespace0
        ),
        |value| Name {
            loc: None,
            value: std::str::from_utf8(value.0).unwrap().to_string(),
        }
    )
);

named!(
    variable<CompleteByteSlice<'_>, Variable>,
    map!(preceded!(call!(punct, "$"), name), |name| Variable {
        loc: None,
        name: name
    })
);

named!(
    integer_part<CompleteByteSlice<'_>, ()>,
    do_parse!(
        opt!(tag!("-"))
            >> alt!(
                tag!("0") => {|_| CompleteByteSlice(b"")} |
                recognize!(
                    do_parse!(
                        one_of!("123456789")
                        >> take_while!(nom::is_digit)
                        >> (())
                    )
                )
            )
            >> (())
    )
);

named!(
    fractional_part<CompleteByteSlice<'_>, ()>,
    map!(preceded!(tag!("."), take_while!(nom::is_digit)), |_| ())
);

named!(
    exponential_part<CompleteByteSlice<'_>, ()>,
    do_parse!(one_of!("eE") >> opt!(one_of!("+-")) >> take_while!(nom::is_digit) >> (()))
);

named!(
    float_value<CompleteByteSlice<'_>, FloatValue>,
    map!(
        delimited!(
            whitespace0,
            recognize!(preceded!(
                integer_part,
                alt!(
                    fractional_part => {|_|""} |
                    exponential_part => {|_|""} |
                    pair!(fractional_part, exponential_part) => {|_|""}
                )
            )),
            whitespace0
        ),
        |value| FloatValue {
            loc: None,
            value: std::str::from_utf8(value.0).unwrap().to_string()
        }
    )
);

#[test]
fn test_float_value() {
    assert_eq!(
        float_value(CompleteByteSlice(b"1.1")),
        Ok((
            CompleteByteSlice(b""),
            FloatValue {
                loc: None,
                value: "1.1".to_string()
            }
        ))
    );
}

named!(
    int_value<CompleteByteSlice<'_>, IntValue>,
    map!(
        delimited!(whitespace0, recognize!(integer_part), whitespace0),
        |value| IntValue {
            loc: None,
            value: std::str::from_utf8(value.0).unwrap().to_string()
        }
    )
);

named!(
    value<CompleteByteSlice<'_>, Value>,
    alt!(
        variable => {|v| Value::Variable(Box::new(v))} |
        float_value => {|v| Value::FloatValue(Box::new(v))} |
        int_value => {|v| Value::IntValue(Box::new(v))}
        // string_value => {|v| Value::BooleanValue(Box::new(v))} |
        // null_value => {|v| Value::NullValue(Box::new(v))} |
        // enum_value => {|v| Value::EnumValue(Box::new(v))} |
        // list_value => {|v| Value::ListValue(Box::new(v))} |
        // object_value => {|v| Value::ObjectValue(Box::new(v))}
    )
);

named!(
    argument<CompleteByteSlice<'_>, Argument>,
    map!(separated_pair!(name, call!(punct, ":"), value), |(
        name,
        value,
    )| {
        Argument {
            loc: None,
            name: name,
            value: value,
        }
    })
);

named!(
    arguments<CompleteByteSlice<'_>, Vec<Argument>>,
    delimited!(call!(punct, "("), many0!(argument), call!(punct, ")"))
);

named!(
    directive<CompleteByteSlice<'_>, Directive>,
    do_parse!(
        call!(punct, "@")
            >> name: name
            >> arguments: opt!(arguments)
            >> (Directive {
                loc: None,
                name: name,
                arguments: arguments,
            })
    )
);

named!(
    directives<CompleteByteSlice<'_>, Vec<Directive>>,
    many1!(directive)
);

named!(
    operation_type<CompleteByteSlice<'_>, OperationType>,
    alt!(
        tag!("query") => {|_| OperationType::QUERY} |
        tag!("mutation") => {|_| OperationType::MUTATION} |
        tag!("subscription") => {|_| OperationType::SUBSCRIPTION}
    )
);

named!(
    named_type<CompleteByteSlice<'_>, NamedType>,
    map!(name, |name| NamedType {
        loc: None,
        name: name
    })
);

named!(
    root_operation_type_definition<CompleteByteSlice<'_>, OperationTypeDefinition>,
    map!(
        separated_pair!(operation_type, call!(punct, ":"), named_type),
        |(operation_type, named_type)| OperationTypeDefinition {
            loc: None,
            operation: operation_type,
            _type: named_type
        }
    )
);

named!(
    schema_definition<CompleteByteSlice<'_>, SchemaDefinition>,
    do_parse!(
        tag!("schema")
            >> directives: opt!(directives)
            >> operation_types:
                delimited!(
                    call!(punct, "{"),
                    many0!(root_operation_type_definition),
                    call!(punct, "}")
                )
            >> (SchemaDefinition {
                loc: None,
                directives: directives,
                operation_types: operation_types
            })
    )
);

#[test]
fn test_schema_definition() {
    assert_eq!(
        schema_definition(CompleteByteSlice(b"schema @abc{}")),
        Ok((
            CompleteByteSlice(b""),
            SchemaDefinition {
                loc: None,
                directives: Some(vec![Directive {
                    loc: None,
                    name: Name {
                        value: "abc".to_string(),
                        loc: None
                    },
                    arguments: None
                }]),
                operation_types: vec![]
            }
        ))
    );
}

named!(
    directive_locations<CompleteByteSlice<'_>, Vec<Name>>,
    preceded!(
        opt!(call!(punct, "|")),
        separated_list!(
            call!(punct, "|"),
            switch!(
                peek!(take_while1!(is_alphanumeric_or_underscore)),
                CompleteByteSlice(b"QUERY")
                | CompleteByteSlice(b"MUTATION")
                | CompleteByteSlice(b"SUBSCRIPTION")
                | CompleteByteSlice(b"FIELD")
                | CompleteByteSlice(b"FRAGMENT_DEFINITION")
                | CompleteByteSlice(b"FRAGMENT_SPREAD")
                | CompleteByteSlice(b"INLINE_FRAGMENT")
                | CompleteByteSlice(b"SCHEMA")
                | CompleteByteSlice(b"SCALAR")
                | CompleteByteSlice(b"OBJECT")
                | CompleteByteSlice(b"FIELD_DEFINITION")
                | CompleteByteSlice(b"ARGUMENT_DEFINITION")
                | CompleteByteSlice(b"INTERFACE")
                | CompleteByteSlice(b"UNION")
                | CompleteByteSlice(b"ENUM")
                | CompleteByteSlice(b"ENUM_VALUE")
                | CompleteByteSlice(b"INPUT_OBJECT")
                | CompleteByteSlice(b"INPUT_FIELD_DEFINITION") => map!(name, |v| v) |
                _ => map!(name, |v| v)
            )
        )
    )
);

named!(
    default_value<CompleteByteSlice<'_>, Value>,
    preceded!(call!(punct, "="), value)
);

named!(
    list_type<CompleteByteSlice<'_>, ListType>,
    map!(
        delimited!(call!(punct, "["), _type, call!(punct, "]")),
        |v| ListType {
            loc: None,
            _type: Box::new(v)
        }
    )
);

named!(
    non_null_type<CompleteByteSlice<'_>, NonNullType>,
    map!(
        terminated!(
            alt!(
                list_type => {|v| NonNullInnerType::ListType(Box::new(v))} |
                named_type => {|v| NonNullInnerType::NamedType(Box::new(v))}
            ),
            tag!("!")
        ),
        |v| NonNullType {
            loc: None,
            _type: v
        }
    )
);

named!(
    _type<CompleteByteSlice<'_>, Type>,
    alt!(
        non_null_type => {|v| Type::NonNullType(Box::new(v))} |
        list_type => {|v| Type::ListType(Box::new(v))} |
        named_type => {|v| Type::NamedType(Box::new(v))}
    )
);

named!(
    input_value_definition<CompleteByteSlice<'_>, InputValueDefinition>,
    do_parse!(
        name: name
            >> call!(punct, ":")
            >> _type: _type
            >> default_value: opt!(default_value)
            >> directives: opt!(directives)
            >> (InputValueDefinition {
                loc: None,
                description: None,
                name: name,
                _type: _type,
                default_value: default_value,
                directives: directives,
            })
    )
);

named!(
    argument_definition<CompleteByteSlice<'_>, Vec<InputValueDefinition>>,
    delimited!(
        call!(punct, "("),
        many1!(input_value_definition),
        call!(punct, ")")
    )
);

named!(
    directive_definition<CompleteByteSlice<'_>, DirectiveDefinition>,
    do_parse!(
        tag!("directive")
            >> call!(punct, "@")
            >> name: name
            >> arguments: opt!(argument_definition)
            >> tag!("on")
            >> locations: directive_locations
            >> (DirectiveDefinition {
                loc: None,
                description: None,
                name: name,
                arguments: arguments,
                locations: locations,
            })
    )
);

named!(
    union_definition<CompleteByteSlice<'_>, UnionTypeDefinition>,
    do_parse!(
        tag!("union")
            >> name: name
            >> directives: opt!(directives)
            >> union_member_types:
                opt!(preceded!(
                    opt!(call!(punct, "|")),
                    separated_list!(call!(punct, "|"), named_type)
                ))
            >> (UnionTypeDefinition {
                loc: None,
                description: None,
                name: name,
                directives: directives,
                types: union_member_types
            })
    )
);

named!(
    scalar_definition<CompleteByteSlice<'_>, ScalarTypeDefinition>,
    do_parse!(
        tag!("scalar")
            >> name: name
            >> directives: opt!(directives)
            >> (ScalarTypeDefinition {
                loc: None,
                description: None,
                name: name,
                directives: directives
            })
    )
);

named!(
    field_definition<CompleteByteSlice<'_>, FieldDefinition>,
    do_parse!(
        name: name
            >> argument_definition: opt!(argument_definition)
            >> call!(punct, ":")
            >> _type: _type
            >> directives: opt!(directives)
            >> (FieldDefinition {
                loc: None,
                description: None,
                name: name,
                arguments: argument_definition,
                _type: _type,
                directives: directives
            })
    )
);

named!(
    fields_definition<CompleteByteSlice<'_>, Vec<FieldDefinition>>,
    delimited!(
        call!(punct, "{"),
        many0!(field_definition),
        call!(punct, "}")
    )
);

named!(
    interface_definition<CompleteByteSlice<'_>, InterfaceTypeDefinition>,
    do_parse!(
        tag!("interface")
            >> name: name
            >> directives: opt!(directives)
            >> fields_definition: opt!(fields_definition)
            >> (InterfaceTypeDefinition {
                loc: None,
                description: None,
                name: name,
                directives: directives,
                fields: fields_definition
            })
    )
);

named!(
    input_fields_definition<CompleteByteSlice<'_>, Vec<InputValueDefinition>>,
    delimited!(
        call!(punct, "{"),
        many0!(input_value_definition),
        call!(punct, "}")
    )
);

named!(
    input_definition<CompleteByteSlice<'_>, InputObjectTypeDefinition>,
    do_parse!(
        tag!("input")
            >> name: name
            >> directives: opt!(directives)
            >> fields: opt!(input_fields_definition)
            >> (InputObjectTypeDefinition {
                loc: None,
                description: None,
                name: name,
                directives: directives,
                fields: fields
            })
    )
);

named!(
    object_definition<CompleteByteSlice<'_>, ObjectTypeDefinition>,
    do_parse!(
        tag!("type")
            >> name: name
            >> implements_interfaces:
                opt!(do_parse!(
                    tag!("implements")
                        >> interfaces:
                            preceded!(opt!(tag!("&")), separated_list!(tag!("&"), named_type))
                        >> (interfaces)
                ))
            >> directives: opt!(directives)
            >> fields_definition: opt!(fields_definition)
            >> (ObjectTypeDefinition {
                loc: None,
                description: None,
                name: name,
                interfaces: implements_interfaces,
                directives: directives,
                fields: fields_definition
            })
    )
);

named!(
    type_definition<CompleteByteSlice<'_>, TypeDefinition>,
    switch!(
        peek!(take_while1!(nom::is_alphanumeric)),
        CompleteByteSlice(b"type") => map!(map!(object_definition, Box::new), TypeDefinition::ObjectTypeDefinition)
        | CompleteByteSlice(b"interface") => map!(map!(interface_definition, Box::new), TypeDefinition::InterfaceTypeDefinition)
        | CompleteByteSlice(b"scalar") => map!(map!(scalar_definition, Box::new), TypeDefinition::ScalarTypeDefinition)
        | CompleteByteSlice(b"union") => map!(map!(union_definition, Box::new), TypeDefinition::UnionTypeDefinition)
        | CompleteByteSlice(b"enum") => map!(map!(enum_definition, Box::new), TypeDefinition::EnumTypeDefinition)
        | CompleteByteSlice(b"input") => map!(map!(input_definition, Box::new), TypeDefinition::InputObjectTypeDefinition)
        | _ => map!(map!(input_definition, Box::new), TypeDefinition::InputObjectTypeDefinition)
    )
);

named!(
    enum_definition<CompleteByteSlice<'_>, EnumTypeDefinition>,
    do_parse!(
        tag!("enum")
            >> name_value: name
            >> directives_value: opt!(directives)
            >> values:
                opt!(delimited!(
                    call!(punct, "{"),
                    many0!(do_parse!(
                        v: name
                            >> directives2: opt!(directives)
                            >> (EnumValueDefinition {
                                loc: None,
                                description: None,
                                name: v,
                                directives: directives2
                            })
                    )),
                    call!(punct, "}")
                ))
            >> (EnumTypeDefinition {
                loc: None,
                description: None,
                name: name_value,
                directives: directives_value,
                values: values
            })
    )
);

named!(
    type_system_definition<CompleteByteSlice<'_>, TypeSystemDefinition>,
    switch!(peek!(take_while1!(nom::is_alphanumeric)),
        CompleteByteSlice(b"schema") => map!(map!(schema_definition, Box::new), TypeSystemDefinition::SchemaDefinition) |
        CompleteByteSlice(b"directive") => map!(map!(directive_definition, Box::new), TypeSystemDefinition::DirectiveDefinition) |
        CompleteByteSlice(b"type")
        | CompleteByteSlice(b"interface")
        | CompleteByteSlice(b"scalar")
        | CompleteByteSlice(b"union")
        | CompleteByteSlice(b"enum")
        | CompleteByteSlice(b"input") => map!(map!(type_definition, Box::new), TypeSystemDefinition::TypeDefinition)
        | _ => map!(map!(schema_definition, Box::new), TypeSystemDefinition::SchemaDefinition)
    )
);

#[test]
fn test_type_system_definition() {
    assert_eq!(
        type_system_definition(CompleteByteSlice(b"schema {}")),
        Ok((
            CompleteByteSlice(b""),
            TypeSystemDefinition::SchemaDefinition(Box::new(SchemaDefinition {
                loc: None,
                directives: None,
                operation_types: vec![]
            }))
        ))
    );
}

named!(
    definition<CompleteByteSlice<'_>, Definition>,
    switch!(peek!(take_while1!(nom::is_alphanumeric)),
        CompleteByteSlice(b"schema")
        | CompleteByteSlice(b"type")
        | CompleteByteSlice(b"interface")
        | CompleteByteSlice(b"scalar")
        | CompleteByteSlice(b"union")
        | CompleteByteSlice(b"enum")
        | CompleteByteSlice(b"input")
        | CompleteByteSlice(b"directive") => map!(map!(type_system_definition, Box::new), Definition::TypeSystemDefinition) |
        _ => map!(map!(type_system_definition, Box::new), Definition::TypeSystemDefinition)
    )
);

#[test]
fn test_definition() {
    assert_eq!(
        definition(CompleteByteSlice(b"schema {}")),
        Ok((
            CompleteByteSlice(b""),
            Definition::TypeSystemDefinition(Box::new(TypeSystemDefinition::SchemaDefinition(
                Box::new(SchemaDefinition {
                    loc: None,
                    directives: None,
                    operation_types: vec![]
                })
            )))
        ))
    );
}

named!(
    document<CompleteByteSlice<'_>, Document>,
    do_parse!(
        whitespace0
            >> definitions: many1!(definition)
            >> (Document {
                loc: None,
                definitions: definitions,
            })
    )
);

#[test]
fn test_document_1() {
    assert_eq!(
        document(CompleteByteSlice(b"schema {}")),
        Ok((
            CompleteByteSlice(b""),
            Document {
                loc: None,
                definitions: vec![Definition::TypeSystemDefinition(Box::new(
                    TypeSystemDefinition::SchemaDefinition(Box::new(SchemaDefinition {
                        loc: None,
                        directives: None,
                        operation_types: vec![]
                    }))
                ))],
            }
        ))
    );
}

/// Takes a graphql source representation and returns the parsed document.
/// ```
/// # use graphql_rs_native::language::parser::parse;
/// # use graphql_rs_native::language::source::Source;
/// let document = parse(Source::new("type User { id: ID }".to_string(), None, None));
/// assert_eq!(document.definitions.len(), 1);
/// ```
pub fn parse(source: Source) -> Document {
    let parse_result = document(CompleteByteSlice(source.body.as_bytes())).unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

impl From<String> for Document {
    fn from(string: String) -> Document {
        Source::new(string, None, None).into()
    }
}

impl From<Source> for Document {
    fn from(source: Source) -> Document {
        parse(source)
    }
}
