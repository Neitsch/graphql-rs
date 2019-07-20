use super::ast::*;
use super::source::Source;
use nom::{
    branch::alt,
    bytes::{
        complete::{take_till, take_while, take_while1},
        streaming::tag,
    },
    character::{complete::one_of, is_alphanumeric, is_digit},
    combinator::{map, opt, peek, recognize},
    error::{ErrorKind, ParseError},
    error_position,
    multi::{many0, many1, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};

/// Takes a graphql source representation and returns the parsed document.
/// ```
/// # use graphql_rs_native::language::parser::parse;
/// # use graphql_rs_native::language::source::Source;
/// let document = parse(Source::new("type User { id: ID }".to_string(), None, None));
/// assert_eq!(document.definitions.len(), 1);
/// ```
pub fn parse(source: Source) -> Document {
    let parse_result = document::<(&str, ErrorKind)>(&source.body)
        .map_err(|e| panic!("{:?}", e))
        .unwrap();
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

fn is_alphanumeric_or_underscope(c: u8) -> bool {
    is_alphanumeric(c) || c == b'_'
}

/// Consumes any form of whitespace until a different character occurs.
fn sp1<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many1(alt((
        map(take_while1(move |c| " \n\r\t".contains(c)), |_| ()),
        map(
            pair(
                take_while1(move |c| "#".contains(c)),
                take_till(move |c| "\n\r".contains(c)),
            ),
            |_| (),
        ),
    ))))(i)
}

#[test]
fn sp1_1() {
    assert_eq!(sp1::<nom::error::VerboseError<&str>>(" "), Ok(("", " ")));
}

#[test]
fn sp1_2() {
    assert_eq!(
        sp1::<nom::error::VerboseError<&str>>(" a "),
        Ok(("a ", " "))
    );
}

/// A special tag for lexical tokens with insignificant whitespaces
fn graphql_tag<'a, E: ParseError<&'a str>>(
    t: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E> {
    move |input: &'a str| delimited(opt(sp1), tag(t), opt(sp1))(input)
}

fn name<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Name, E> {
    map(
        take_while1(|c: char| is_alphanumeric_or_underscope(c as u8)),
        |string: &'a str| Name {
            loc: None,
            value: string.to_string(),
        },
    )(source)
}

fn variable<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Variable, E> {
    map(preceded(graphql_tag("$"), name), |name| Variable {
        loc: None,
        name,
    })(source)
}

fn integer_part<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(
        opt(tag("-")),
        preceded(one_of("123456789"), take_while(|c: char| is_digit(c as u8))),
    )(source)
}

fn fractional_part<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(tag("."), take_while(|c: char| is_digit(c as u8)))(source)
}

fn exponential_part<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(
        one_of("eE"),
        preceded(opt(one_of("+-")), take_while(|c: char| is_digit(c as u8))),
    )(source)
}

fn float_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, FloatValue, E> {
    map(
        recognize(preceded(
            integer_part,
            alt((
                fractional_part,
                exponential_part,
                recognize(pair(fractional_part, exponential_part)),
            )),
        )),
        |res| FloatValue {
            loc: None,
            value: res.to_string(),
        },
    )(source)
}

#[test]
fn test_float_value() {
    assert_eq!(
        float_value::<(&str, ErrorKind)>("1.1"),
        Ok((
            "",
            FloatValue {
                loc: None,
                value: "1.1".to_string()
            }
        ))
    );
}

fn int_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, IntValue, E> {
    map(recognize(integer_part), |val| IntValue {
        loc: None,
        value: val.to_string(),
    })(source)
}
/*
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
*/
fn value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Value, E> {
    alt((
        map(variable, |v| Value::Variable(Box::new(v))),
        map(float_value, |v| Value::FloatValue(Box::new(v))),
        map(int_value, |v| Value::IntValue(Box::new(v))),
    ))(source)
}

fn argument<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Argument, E> {
    map(
        separated_pair(name, graphql_tag(":"), value),
        |(name, value)| Argument {
            loc: None,
            name,
            value,
        },
    )(source)
}

fn arguments<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Vec<Argument>, E> {
    preceded(
        graphql_tag("("),
        terminated(many0(preceded(opt(sp1), argument)), graphql_tag(")")),
    )(source)
}

fn directive<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Directive, E> {
    map(
        preceded(graphql_tag("@"), pair(name, opt(arguments))),
        |(name, args)| Directive {
            loc: None,
            name: name,
            arguments: args,
        },
    )(source)
}

fn directives<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Vec<Directive>, E> {
    many1(directive)(source)
}

fn operation_type<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, OperationType, E> {
    alt((
        map(tag("query"), |_| OperationType::QUERY),
        map(tag("mutation"), |_| OperationType::MUTATION),
        map(tag("subscription"), |_| OperationType::SUBSCRIPTION),
    ))(source)
}

fn named_type<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, NamedType, E> {
    map(name, |name| NamedType { loc: None, name })(source)
}

fn root_operation_type_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, OperationTypeDefinition, E> {
    map(
        separated_pair(operation_type, graphql_tag(":"), named_type),
        |(operation, named_type)| OperationTypeDefinition {
            loc: None,
            operation,
            _type: named_type,
        },
    )(source)
}

fn schema_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, SchemaDefinition, E> {
    map(
        preceded(
            tag("schema"),
            pair(
                opt(directives),
                preceded(
                    graphql_tag("{"),
                    terminated(many0(root_operation_type_definition), graphql_tag("}")),
                ),
            ),
        ),
        |(directives, operation_types)| SchemaDefinition {
            loc: None,
            directives,
            operation_types,
        },
    )(source)
}

#[test]
fn test_schema_definition() {
    assert_eq!(
        schema_definition::<(&str, ErrorKind)>("schema @abc {}"),
        Ok((
            "",
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

fn directive_locations<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<Name>, E> {
    preceded(
        opt(graphql_tag("|")),
        separated_list(graphql_tag("!"), move |input: &'a str| {
            let (rest, loc) = peek(take_while1(|c: char| {
                is_alphanumeric_or_underscope(c as u8)
            }))(input)?;
            match loc {
                "QUERY"
                | "MUTATION"
                | "SUBSCRIPTION"
                | "FIELD"
                | "FRAGMENT_DEFINITION"
                | "FRAGMENT_SPREAD"
                | "INLINE_FRAGMENT"
                | "SCHEMA"
                | "SCALAR"
                | "OBJECT"
                | "FIELD_DEFINITION"
                | "ARGUMENT_DEFINITION"
                | "INTERFACE"
                | "UNION"
                | "ENUM"
                | "ENUM_VALUE"
                | "INPUT_OBJECT"
                | "INPUT_FIELD_DEFINITION" => name(loc).map(|(_, name)| (rest, name)),
                _ => panic!("lol1"),
            }
        }),
    )(source)
}

fn default_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Value, E> {
    preceded(graphql_tag("="), value)(source)
}

fn list_type<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, ListType, E> {
    preceded(graphql_tag("["), terminated(type_node, graphql_tag("]")))(source).map(|(rest, v)| {
        (
            rest,
            ListType {
                loc: None,
                _type: Box::new(v),
            },
        )
    })
}

fn non_null_type<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, NonNullType, E> {
    terminated(
        alt((
            move |input: &'a str| {
                list_type(input).map(|(rest, v)| (rest, NonNullInnerType::ListType(Box::new(v))))
            },
            move |input: &'a str| {
                named_type(input).map(|(rest, v)| (rest, NonNullInnerType::NamedType(Box::new(v))))
            },
        )),
        graphql_tag("!"),
    )(source)
    .map(|(rest, v)| {
        (
            rest,
            NonNullType {
                loc: None,
                _type: v,
            },
        )
    })
}

fn type_node<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Type, E> {
    alt((
        move |input: &'a str| {
            non_null_type(input).map(|(rest, v)| (rest, Type::NonNullType(Box::new(v))))
        },
        move |input: &'a str| list_type(input).map(|(rest, v)| (rest, Type::ListType(Box::new(v)))),
        move |input: &'a str| {
            named_type(input).map(|(rest, v)| (rest, Type::NamedType(Box::new(v))))
        },
    ))(source)
}

fn input_value_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, InputValueDefinition, E> {
    let (source, name) = name(source)?;
    let (source, _) = graphql_tag(":")(source)?;
    let (source, type_node) = type_node(source)?;
    let (source, default_value) = opt(default_value)(source)?;
    opt(directives)(source).map(|(rest, directives)| {
        (
            rest,
            InputValueDefinition {
                loc: None,
                description: None,
                name,
                _type: type_node,
                default_value,
                directives,
            },
        )
    })
}

fn argument_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<InputValueDefinition>, E> {
    preceded(
        graphql_tag("("),
        terminated(many1(input_value_definition), graphql_tag(")")),
    )(source)
}

fn directive_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, DirectiveDefinition, E> {
    let (source, _) = tag("directive")(source)?;
    let (source, _) = graphql_tag("@")(source)?;
    let (source, name) = name(source)?;
    let (source, arguments) = opt(argument_definition)(source)?;
    let (source, _) = tag("on")(source)?;
    directive_locations(source).map(|(rest, locations)| {
        (
            rest,
            DirectiveDefinition {
                loc: None,
                description: None,
                name,
                arguments,
                locations,
            },
        )
    })
}

fn union_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, UnionTypeDefinition, E> {
    let (source, _) = tag("union")(source)?;
    let (source, name) = name(source)?;
    let (source, directives) = opt(directives)(source)?;
    opt(preceded(
        opt(graphql_tag("|")),
        separated_list(graphql_tag("|"), named_type),
    ))(source)
    .map(|(rest, union_member_types)| {
        (
            rest,
            UnionTypeDefinition {
                loc: None,
                description: None,
                name,
                directives,
                types: union_member_types,
            },
        )
    })
}

fn scalar_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, ScalarTypeDefinition, E> {
    let (source, _) = tag("scalar")(source)?;
    let (source, name) = name(source)?;
    opt(directives)(source).map(|(rest, directives)| {
        (
            rest,
            ScalarTypeDefinition {
                loc: None,
                description: None,
                name,
                directives,
            },
        )
    })
}

fn field_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, FieldDefinition, E> {
    let (source, name) = name(source)?;
    let (source, argument_definition) = opt(argument_definition)(source)?;
    let (source, _) = graphql_tag(":")(source)?;
    let (source, type_node) = type_node(source)?;
    opt(directives)(source).map(|(rest, directives)| {
        (
            rest,
            FieldDefinition {
                loc: None,
                description: None,
                name,
                arguments: argument_definition,
                _type: type_node,
                directives,
            },
        )
    })
}

fn fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<FieldDefinition>, E> {
    preceded(
        graphql_tag("{"),
        terminated(
            many0(preceded(opt(sp1), field_definition)),
            graphql_tag("}"),
        ),
    )(source)
}
/*
type Author {
    id: Int!
    firstName: String
    lastName: String
    posts: [Post]
}
*/
#[test]
fn test_fields_definition_x() {
    assert_eq!(
        fields_definition::<nom::error::VerboseError<&str>>("{f:S l:S }"),
        Ok((
            "",
            vec![
                FieldDefinition {
                    loc: None,
                    description: None,
                    name: Name {
                        loc: None,
                        value: "f".to_string(),
                    },
                    arguments: None,
                    _type: Type::NamedType(Box::new(NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "S".to_string(),
                        },
                    })),
                    directives: None,
                },
                FieldDefinition {
                    loc: None,
                    description: None,
                    name: Name {
                        loc: None,
                        value: "l".to_string(),
                    },
                    arguments: None,
                    _type: Type::NamedType(Box::new(NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "S".to_string(),
                        },
                    })),
                    directives: None,
                },
            ]
        ))
    );
}

#[test]
fn test_fields_definition() {
    assert_eq!(
        fields_definition::<(&str, ErrorKind)>("{ id: ID }"),
        Ok((
            "",
            vec![FieldDefinition {
                loc: None,
                description: None,
                name: Name {
                    loc: None,
                    value: "id".to_string()
                },
                arguments: None,
                _type: Type::NamedType(Box::new(NamedType {
                    loc: None,
                    name: Name {
                        loc: None,
                        value: "ID".to_string()
                    }
                })),
                directives: None,
            }]
        ))
    )
}

fn interface_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, InterfaceTypeDefinition, E> {
    let (source, _) = tag("interface")(source)?;
    let (source, name) = name(source)?;
    let (source, directives) = opt(directives)(source)?;
    opt(fields_definition)(source).map(|(rest, fd)| {
        (
            rest,
            InterfaceTypeDefinition {
                loc: None,
                description: None,
                name,
                directives,
                fields: fd,
            },
        )
    })
}

fn input_fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<InputValueDefinition>, E> {
    preceded(
        graphql_tag("{"),
        terminated(many0(input_value_definition), graphql_tag("}")),
    )(source)
}

fn input_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, InputObjectTypeDefinition, E> {
    let (source, _) = tag("input")(source)?;
    let (source, name) = name(source)?;
    let (source, directives) = opt(directives)(source)?;
    opt(input_fields_definition)(source).map(|(rest, ifd)| {
        (
            rest,
            InputObjectTypeDefinition {
                loc: None,
                description: None,
                name,
                directives,
                fields: ifd,
            },
        )
    })
}

fn object_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, ObjectTypeDefinition, E> {
    let (source, _) = terminated(tag("type"), opt(sp1))(source)?;
    let (source, name_value) = name(source)?;
    let (source, implements_interfaces) = opt(move |input: &'a str| {
        let (rest, _) = tag("implements")(input)?;
        preceded(opt(tag("&")), separated_list(tag("&"), named_type))(rest)
    })(source)?;
    let (source, directives) = opt(directives)(source)?;
    opt(fields_definition)(source).map(|(rest, fd)| {
        (
            rest,
            ObjectTypeDefinition {
                loc: None,
                description: None,
                name: name_value,
                interfaces: implements_interfaces,
                directives,
                fields: fd,
            },
        )
    })
}

#[test]
fn test_object_definition() {
    assert_eq!(
        object_definition::<(&str, ErrorKind)>("type User { id: ID }"),
        Ok((
            "",
            ObjectTypeDefinition {
                loc: None,
                description: None,
                name: Name {
                    loc: None,
                    value: "User".to_string(),
                },
                interfaces: None,
                directives: None,
                fields: Some(vec![FieldDefinition {
                    loc: None,
                    description: None,
                    name: Name {
                        loc: None,
                        value: "id".to_string()
                    },
                    arguments: None,
                    _type: Type::NamedType(Box::new(NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "ID".to_string()
                        }
                    })),
                    directives: None,
                }])
            }
        ))
    );
}

fn type_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, TypeDefinition, E> {
    let (source, prefix) = peek(take_while1(|c: char| is_alphanumeric(c as u8)))(source)?;
    match prefix {
        "type" => object_definition(source).map(|(rest, graphql_object)| {
            (
                rest,
                TypeDefinition::ObjectTypeDefinition(Box::new(graphql_object)),
            )
        }),
        "interface" => interface_definition(source).map(|(rest, graphql_interface)| {
            (
                rest,
                TypeDefinition::InterfaceTypeDefinition(Box::new(graphql_interface)),
            )
        }),
        "scalar" => scalar_definition(source).map(|(rest, graphql_scalar)| {
            (
                rest,
                TypeDefinition::ScalarTypeDefinition(Box::new(graphql_scalar)),
            )
        }),
        "union" => union_definition(source).map(|(rest, graphql_union)| {
            (
                rest,
                TypeDefinition::UnionTypeDefinition(Box::new(graphql_union)),
            )
        }),
        "enum" => enum_definition(source).map(|(rest, graphql_enum)| {
            (
                rest,
                TypeDefinition::EnumTypeDefinition(Box::new(graphql_enum)),
            )
        }),
        "input" => input_definition(source).map(|(rest, graphql_input)| {
            (
                rest,
                TypeDefinition::InputObjectTypeDefinition(Box::new(graphql_input)),
            )
        }),
        _ => panic!("lol2"),
    }
}

fn enum_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, EnumTypeDefinition, E> {
    let (source, _) = tag("enum")(source)?;
    let (source, name_value) = name(source)?;
    let (source, directives_value) = opt(directives)(source)?;
    opt(preceded(
        graphql_tag("{"),
        terminated(
            many0(move |input: &'a str| {
                let (rest, n) = name(input)?;
                opt(directives)(rest).map(|(rest, d)| {
                    (
                        rest,
                        EnumValueDefinition {
                            loc: None,
                            description: None,
                            name: n,
                            directives: d,
                        },
                    )
                })
            }),
            graphql_tag("}"),
        ),
    ))(source)
    .map(|(rest, values)| {
        (
            rest,
            EnumTypeDefinition {
                loc: None,
                description: None,
                name: name_value,
                directives: directives_value,
                values,
            },
        )
    })
}

fn type_system_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, TypeSystemDefinition, E> {
    let res: IResult<&'a str, &'a str, E> =
        peek(take_while1(|c: char| is_alphanumeric(c as u8)))(source);
    let (rest, prefix) = res.unwrap_or_else(|_| panic!("lol3"));
    match prefix {
        "schema" => schema_definition(rest)
            .map(|(rest, sd)| (rest, TypeSystemDefinition::SchemaDefinition(Box::new(sd)))),
        "directive" => directive_definition(rest).map(|(rest, dd)| {
            (
                rest,
                TypeSystemDefinition::DirectiveDefinition(Box::new(dd)),
            )
        }),
        "type" | "interface" | "scalar" | "union" | "enum" | "input" => type_definition(rest)
            .map(|(rest, td)| (rest, TypeSystemDefinition::TypeDefinition(Box::new(td)))),
        _ => Err(nom::Err::Failure(error_position!(
            source,
            ErrorKind::TagBits
        ))),
    }
}

#[test]
fn test_type_system_definition() {
    assert_eq!(
        type_system_definition::<(&str, ErrorKind)>("schema {}"),
        Ok((
            "",
            TypeSystemDefinition::SchemaDefinition(Box::new(SchemaDefinition {
                loc: None,
                directives: None,
                operation_types: vec![]
            }))
        ))
    );
}

fn definition<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Definition, E> {
    peek(take_while1(|c: char| is_alphanumeric(c as u8)))(source).and_then(|(rest, prefix)| {
        match prefix {
            "schema" | "type" | "interface" | "scalar" | "union" | "enum" | "input"
            | "directive" => type_system_definition(rest)
                .map(|(rest, tsd)| (rest, Definition::TypeSystemDefinition(Box::new(tsd)))),
            _ => panic!("lol4"),
        }

    })
}

#[test]
fn test_definition() {
    assert_eq!(
        definition::<(&str, ErrorKind)>("schema {}"),
        Ok((
            "",
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

fn document<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Document, E> {
    preceded(
        opt(sp1),
        map(many1(definition), |definitions| Document {
            definitions,
            loc: None,
        }),
    )(source)
}

#[test]
fn test_document_1() {
    assert_eq!(
        document::<(&str, ErrorKind)>("schema {}"),
        Ok((
            "",
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

#[test]
fn test_document_2() {
    assert_eq!(
        document::<nom::error::VerboseError<&str>>(
            "
type Author {
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

# the schema allows the following query:
type Query {
    posts: [Post]
    author(id: Int!): Author
}

# this schema allows the following mutation:
type Mutation {
    upvotePost (
    postId: Int!
    ): Post
}
        "
        ),
        Ok((
            "",
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
