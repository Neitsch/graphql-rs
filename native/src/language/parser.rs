extern crate nom;

use nom::types::CompleteByteSlice;

use super::ast::*;
use super::source::Source;

fn whitespace(c: u8) -> bool {
    (c == (' ' as u8)) || (c == ('\t' as u8)) || (c == ('\n' as u8)) || (c == (',' as u8))
}

named!(whitespace0<CompleteByteSlice, CompleteByteSlice>,
    recognize!(
        many0!(
            alt!(
                do_parse!(
                    tag!("#")
                    >> take_until!("\n")
                    >> (())
                ) => {|_| ()} |
                take_while!(whitespace) => {|_| ()}
            )
        )
    )
);

#[test]
fn test_whitespace() {
    assert_eq!(
        whitespace0(CompleteByteSlice(b" # test\n")),
        Ok((
            CompleteByteSlice(b""),
            CompleteByteSlice(b" # test\n")
        ))
    );
}

fn is_alphanumeric_or_underscore(c: u8) -> bool {
    nom::is_alphanumeric(c) || (c == ('_' as u8))
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

named!(name<CompleteByteSlice, Name>,
    map!(delimited!(whitespace0, take_while1!(is_alphanumeric_or_underscore), whitespace0), |value| Name {
        loc: None,
        value: std::str::from_utf8(value.0).unwrap().to_string(),
    })
);

named!(variable<CompleteByteSlice, Variable>,
    map!(preceded!(call!(punct, "$"), name), |name| Variable {loc: None, name: name})
);

named!(integer_part<CompleteByteSlice, ()>,
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
    fractional_part<CompleteByteSlice, ()>,
    map!(preceded!(tag!("."), take_while!(nom::is_digit)), |_| (()))
);

named!(
    exponential_part<CompleteByteSlice, ()>,
    do_parse!(
        one_of!("eE")
        >> opt!(one_of!("+-"))
        >> take_while!(nom::is_digit)
        >> (())
    )
);

named!(
    float_value<CompleteByteSlice, FloatValue>,
    map!(delimited!(whitespace0, recognize!(preceded!(integer_part, alt!(
        fractional_part => {|_|""} |
        exponential_part => {|_|""} |
        pair!(fractional_part, exponential_part) => {|_|""}
    ))), whitespace0), |value| FloatValue {loc: None, value: std::str::from_utf8(value.0).unwrap().to_string()})
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

named!(int_value<CompleteByteSlice, IntValue>,
    map!(delimited!(whitespace0, recognize!(integer_part), whitespace0), |value| IntValue {loc: None, value: std::str::from_utf8(value.0).unwrap().to_string()})
);

named!(value<CompleteByteSlice, Value>,
    alt!(
        variable => {Value::Variable} |
        float_value => {Value::FloatValue} |
        int_value => {Value::IntValue}
        // string_value => {Value::BooleanValue} |
        // null_value => {Value::NullValue} |
        // enum_value => {Value::EnumValue} |
        // list_value => {Value::ListValue} |
        // object_value => {Value::ObjectValue}
    )
);

named!(argument<CompleteByteSlice, Argument>,
    map!(separated_pair!(
        name,
        call!(punct,  ":"),
        value
    ), |(name, value)| Argument {
        loc: None,
        name: name,
        value: value,
    })
);

named!(
    arguments<CompleteByteSlice, Vec<Argument>>,
    delimited!(call!(punct,  "("), many0!(argument), call!(punct,  ")"))
);

named!(directive<CompleteByteSlice, Directive>,
    do_parse!(
        call!(punct,  "@")
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
    directives<CompleteByteSlice, Vec<Directive>>,
    many1!(directive)
);

named!(operation_type<CompleteByteSlice, OperationType>,
    alt!(
        tag!("query") => {|_| OperationType::QUERY} |
        tag!("mutation") => {|_| OperationType::MUTATION} |
        tag!("subscription") => {|_| OperationType::SUBSCRIPTION}
    )
);

named!(named_type<CompleteByteSlice, NamedType>,
    map!(name, |name| NamedType {
        loc: None,
        name: name
    })
);

named!(
    root_operation_type_definition<CompleteByteSlice, OperationTypeDefinition>,
    map!(separated_pair!(
        operation_type,
        call!(punct,  ":"),
        named_type
    ), |(operation_type, named_type)| OperationTypeDefinition{
        loc: None,
        operation: operation_type,
        _type: named_type
    })
);

named!(schema_definition<CompleteByteSlice, SchemaDefinition>,
    do_parse!(
        tag!("schema")
        >> directives: opt!(directives)
        >> operation_types: delimited!(
            call!(punct,  "{"),
            many0!(root_operation_type_definition),
            call!(punct,  "}")
        )
        >> (SchemaDefinition {
            loc: None, directives: directives, operation_types: operation_types
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
    directive_locations<CompleteByteSlice, Vec<Name>>,
    preceded!(
        opt!(call!(punct,  "|")),
        separated_list!(
            call!(punct,  "|"),
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
    default_value<CompleteByteSlice, Value>,
    preceded!(call!(punct,  "="), value)
);

named!(list_type<CompleteByteSlice, ListType>,
    map!(delimited!(
        call!(punct,  "["),
        _type,
        call!(punct,  "]")
    ), |v| ListType {
        loc: None,
        _type: Box::new(v)
    })
);

named!(
    non_null_type<CompleteByteSlice, NonNullType>, map!(
            terminated!(
                alt!(
                    list_type => {NonNullInnerType::ListType} |
                    named_type => {NonNullInnerType::NamedType}
                ),
                tag!("!")
            ),
            |v| NonNullType {
                loc: None,
                _type: v
            }
        )
);

named!(_type<CompleteByteSlice, Type>,
    alt!(
        non_null_type => {Type::NonNullType} |
        list_type => {Type::ListType} |
        named_type => {Type::NamedType}
    )
);

named!(
    input_value_definition<CompleteByteSlice, InputValueDefinition>,
    do_parse!(
        name: name
        >> call!(punct,  ":")
        >> _type: _type
        >> default_value: opt!(default_value)
        >> directives: opt!(directives)
        >> (
            InputValueDefinition {
                loc: None,
                description: None,
                name: name,
                _type: _type,
                default_value: default_value,
                directives: directives,
            }
        )
    )
);

named!(argument_definition<CompleteByteSlice, Vec<InputValueDefinition>>,
    delimited!(
        call!(punct,  "("),
        many1!(input_value_definition),
        call!(punct,  ")")
    )
);

named!(directive_definition<CompleteByteSlice, DirectiveDefinition>,
    do_parse!(
        tag!("directive")
        >> call!(punct,  "@")
        >> name: name
        >> arguments: opt!(argument_definition)
        >> tag!("on")
        >> locations: directive_locations
        >> (
            DirectiveDefinition {
                loc: None,
                description: None,
                name: name,
                arguments: arguments,
                locations: locations,
            }
        )
    )
);

named!(
    union_definition<CompleteByteSlice, UnionTypeDefinition>,
    do_parse!(
        tag!("union")
        >> name: name
        >> directives: opt!(directives)
        >> union_member_types: opt!(
            preceded!(opt!(call!(punct,  "|")),
                separated_list!(
                    call!(punct,  "|"),
                    named_type
                )
            )
        )
        >> (UnionTypeDefinition{
            loc: None,
            description: None,
            name: name,
            directives: directives,
            types: union_member_types
        })
    )
);

named!(
    scalar_definition<CompleteByteSlice, ScalarTypeDefinition>,
    do_parse!(
        tag!("scalar")
        >> name: name
        >> directives: opt!(directives)
        >> (
            ScalarTypeDefinition {
                loc: None,
                description: None,
                name: name,
                directives: directives
            }
        )
    )
);

named!(
    field_definition<CompleteByteSlice, FieldDefinition>,
    do_parse!(
        name: name
        >> argument_definition: opt!(argument_definition)
        >> call!(punct,  ":")
        >> _type: _type
        >> directives: opt!(directives)
        >> (
            FieldDefinition {
                loc: None,
                description: None,
                name: name,
                arguments: argument_definition,
                _type: _type,
                directives: directives
            }
        )
    )
);

named!(
    fields_definition<CompleteByteSlice, Vec<FieldDefinition>>,
    delimited!(
        call!(punct,  "{"),
        many0!(
            field_definition
        ),
        call!(punct,  "}")
    )
);

named!(
    interface_definition<CompleteByteSlice, InterfaceTypeDefinition>,
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
    input_fields_definition<CompleteByteSlice, Vec<InputValueDefinition>>,
    delimited!(
        call!(punct,  "{"),
        many0!(input_value_definition),
        call!(punct,  "}")
    )
);

named!(
    input_definition<CompleteByteSlice, InputObjectTypeDefinition>,
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
    object_definition<CompleteByteSlice, ObjectTypeDefinition>,
    do_parse!(
        tag!("type")
        >> name: name
        >> implements_interfaces: opt!(
            do_parse!(
                tag!("implements")
                >> interfaces: preceded!(
                    opt!(tag!("&")),
                    separated_list!(
                        tag!("&"),
                        named_type
                    )
                )
                >> (interfaces)
            )
        )
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

#[test]
fn test_object_definition() {
    assert_eq!(
        object_definition(CompleteByteSlice(b"type Author {
            id: Int!
            firstName: String
            lastName: String
            posts: [Post]
        }
        ")), Ok((
            CompleteByteSlice(b""),
            ObjectTypeDefinition {
                loc: None,
                description: None,
                name: Name {value:"Author".to_string(), loc: None},
                interfaces: None,
                directives: None,
                fields: None
            }
        ))
    );
}

named!(
    type_definition<CompleteByteSlice, TypeDefinition>,
    switch!(
        peek!(take_while1!(nom::is_alphanumeric)),
        CompleteByteSlice(b"type") => map!(object_definition, TypeDefinition::ObjectTypeDefinition)
        | CompleteByteSlice(b"interface") => map!(interface_definition, TypeDefinition::InterfaceTypeDefinition)
        | CompleteByteSlice(b"scalar") => map!(scalar_definition, TypeDefinition::ScalarTypeDefinition)
        | CompleteByteSlice(b"union") => map!(union_definition, TypeDefinition::UnionTypeDefinition)
        | CompleteByteSlice(b"enum") => map!(enum_definition, TypeDefinition::EnumTypeDefinition)
        | CompleteByteSlice(b"input") => map!(input_definition, TypeDefinition::InputObjectTypeDefinition)
        | _ => map!(input_definition, TypeDefinition::InputObjectTypeDefinition)
    )
);

named!(
    enum_definition<CompleteByteSlice, EnumTypeDefinition>,
    do_parse!(
        tag!("enum")
        >> name_value: name
        >> directives_value: opt!(directives)
        >> values: opt!(delimited!(
            call!(punct,  "{"),
            many0!(do_parse!(
                v: name
                >> directives2: opt!(directives)
                >> (
                    EnumValueDefinition {
                        loc: None,
                        description: None,
                        name: v,
                        directives: directives2
                    }
                )
            )),
            call!(punct,  "}")
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

named!(type_system_definition<CompleteByteSlice, TypeSystemDefinition>,
    switch!(peek!(take_while1!(nom::is_alphanumeric)),
        CompleteByteSlice(b"schema") => map!(schema_definition, TypeSystemDefinition::SchemaDefinition) |
        CompleteByteSlice(b"directive") => map!(directive_definition, TypeSystemDefinition::DirectiveDefinition) |
        CompleteByteSlice(b"type")
        | CompleteByteSlice(b"interface")
        | CompleteByteSlice(b"scalar")
        | CompleteByteSlice(b"union")
        | CompleteByteSlice(b"enum")
        | CompleteByteSlice(b"input") => map!(type_definition, TypeSystemDefinition::TypeDefinition)
        | _ => map!(schema_definition, TypeSystemDefinition::SchemaDefinition)
    )
);

#[test]
fn test_type_system_definition() {
    assert_eq!(
        type_system_definition(CompleteByteSlice(b"schema {}")),
        Ok((
            CompleteByteSlice(b""),
            TypeSystemDefinition::SchemaDefinition(SchemaDefinition {
                loc: None,
                directives: None,
                operation_types: vec![]
            })
        ))
    );
}

named!(definition<CompleteByteSlice, Definition>,
    switch!(peek!(take_while1!(nom::is_alphanumeric)),
        CompleteByteSlice(b"schema")
        | CompleteByteSlice(b"type")
        | CompleteByteSlice(b"interface")
        | CompleteByteSlice(b"scalar")
        | CompleteByteSlice(b"union")
        | CompleteByteSlice(b"enum")
        | CompleteByteSlice(b"input")
        | CompleteByteSlice(b"directive") => map!(type_system_definition, Definition::TypeSystemDefinition) |
        _ => map!(type_system_definition, Definition::TypeSystemDefinition)
    )
);

#[test]
fn test_definition() {
    assert_eq!(
        definition(CompleteByteSlice(b"schema {}")),
        Ok((
            CompleteByteSlice(b""),
            Definition::TypeSystemDefinition(TypeSystemDefinition::SchemaDefinition(
                SchemaDefinition {
                    loc: None,
                    directives: None,
                    operation_types: vec![]
                }
            ))
        ))
    );
}

named!(document<CompleteByteSlice, Document>,
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
                definitions: vec![Definition::TypeSystemDefinition(
                    TypeSystemDefinition::SchemaDefinition(SchemaDefinition {
                        loc: None,
                        directives: None,
                        operation_types: vec![]
                    })
                )],
            }
        ))
    );
}

/*#[test]
fn test_document_2() {
    assert_eq!(
        document(CompleteByteSlice(
            b"type Test {  \n test_value: String \n   }"
        )),
        Ok((
            CompleteByteSlice(b""),
            Document {
                loc: None,
                definitions: vec![Definition::TypeSystemDefinition(
                    TypeSystemDefinition::SchemaDefinition(SchemaDefinition {
                        loc: None,
                        directives: None,
                        operation_types: vec![]
                    })
                )],
            }
        ))
    );
}*/

#[test]
fn test_document_3() {
    assert_eq!(document(CompleteByteSlice(
        b"type Author {
    id: Int!
    firstName: String
    lastName: String
    posts: [Post]
  }

  type Post {
    id: Int!
    title: String
    author: Author
    votes: Int
  }

  type Query {
    posts: [Post]
    author(id: Int!): Author
  }

  type Mutation {
    upvotePost (
      postId: Int!
    ): Post
  }
"
    )), Ok((
        CompleteByteSlice(b""),
        Document {
            loc: None,
            definitions: vec![]
        }
    )));
}

pub fn parse(source: Source) -> Document {
    let parse_result = document(CompleteByteSlice(source.body.as_bytes())).unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

impl From<String> for Document {
    fn from(string: String) -> Document {
        return Source::new(string, None, None).into();
    }
}

impl From<Source> for Document {
    fn from(source: Source) -> Document {
        return parse(source);
    }
}
