use super::ast::*;
use super::source::Source;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::{complete::one_of, is_alphanumeric, is_digit},
    combinator::{map, opt, peek, recognize},
    error::{ErrorKind, ParseError},
    error_position,
    multi::{many0, many1, separated_list, separated_nonempty_list},
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
    let parse_result = document::<(&str, ErrorKind)>(&source.body).map_err(|e| panic!("{:?}", e));
    parse_result.unwrap().1
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
    let chars = " \t\r\n f";

    take_while1(move |c| chars.contains(c))(i)
}

/// A special tag for lexical tokens with insignificant whitespaces
fn graphql_tag<'a, E: ParseError<&'a str>>(
    t: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E> {
    move |input: &'a str| delimited(opt(sp1), tag(t), opt(sp1))(input)
}

fn name<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Name, E> {
    take_while1(|c: char| is_alphanumeric_or_underscope(c as u8))(source).map(|(rest, string)| {
        (
            rest,
            Name {
                loc: None,
                value: string.to_string(),
            },
        )
    })
}

fn variable<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Variable, E> {
    preceded(graphql_tag("$"), name)(source)
        .map(|(rest, name)| (rest, Variable { loc: None, name }))
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
    recognize(preceded(
        integer_part,
        alt((
            fractional_part,
            exponential_part,
            recognize(pair(fractional_part, exponential_part)),
        )),
    ))(source)
    .map(|(rest, res)| {
        (
            rest,
            FloatValue {
                loc: None,
                value: res.to_string(),
            },
        )
    })
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
    recognize(integer_part)(source).map(|(rest, val)| {
        (
            rest,
            IntValue {
                loc: None,
                value: val.to_string(),
            },
        )
    })
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
        move |input: &'a str| {
            let variable_res = variable(input);
            variable_res.map(|(rest, v)| (rest, Value::Variable(Box::new(v))))
        },
        move |input: &'a str| {
            let float_res = float_value(input);
            float_res.map(|(rest, v)| (rest, Value::FloatValue(Box::new(v))))
        },
        move |input: &'a str| {
            let int_res = int_value(input);
            int_res.map(|(rest, v)| (rest, Value::IntValue(Box::new(v))))
        },
    ))(source)
}

fn argument<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Argument, E> {
    separated_pair(name, graphql_tag(":"), value)(source).map(|(rest, (name, value))| {
        (
            rest,
            Argument {
                loc: None,
                name,
                value,
            },
        )
    })
}

fn arguments<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Vec<Argument>, E> {
    preceded(
        graphql_tag("("),
        terminated(separated_list(sp1, argument), graphql_tag(")")),
    )(source)
}

fn directive<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Directive, E> {
    let rest = graphql_tag("@")(source)?.0;
    let (rest, nam) = name(rest)?;
    opt(arguments)(rest).map(|(rest, args)| {
        (
            rest,
            Directive {
                loc: None,
                name: nam,
                arguments: args,
            },
        )
    })
}

fn directives<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Vec<Directive>, E> {
    many1(directive)(source)
}

fn operation_type<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, OperationType, E> {
    alt((
        move |input: &'a str| tag("query")(input).map(|(rest, _)| (rest, OperationType::QUERY)),
        move |input: &'a str| {
            tag("mutation")(input).map(|(rest, _)| (rest, OperationType::MUTATION))
        },
        move |input: &'a str| {
            tag("subscription")(input).map(|(rest, _)| (rest, OperationType::SUBSCRIPTION))
        },
    ))(source)
}

fn named_type<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, NamedType, E> {
    name(source).map(|(rest, name)| (rest, NamedType { loc: None, name }))
}

fn root_operation_type_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, OperationTypeDefinition, E> {
    separated_pair(operation_type, graphql_tag(":"), named_type)(source).map(
        |(rest, (operation, named_type))| {
            (
                rest,
                OperationTypeDefinition {
                    loc: None,
                    operation,
                    _type: named_type,
                },
            )
        },
    )
}

fn schema_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, SchemaDefinition, E> {
    let (source, _) = tag("schema")(source)?;
    let (source, directives) = opt(directives)(source)?;
    preceded(
        graphql_tag("{"),
        terminated(many0(root_operation_type_definition), graphql_tag("}")),
    )(source)
    .map(|(rest, operation_types)| {
        (
            rest,
            SchemaDefinition {
                loc: None,
                directives,
                operation_types,
            },
        )
    })
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
                _ => panic!("lol"),
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
        graphql_tag("{"),
        terminated(many1(input_value_definition), graphql_tag("}")),
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
        terminated(many0(field_definition), graphql_tag("}")),
    )(source)
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
    let (source, _) = terminated(tag("type"), sp1)(source)?;
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
        _ => panic!("lol"),
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
    let (rest, prefix) = res.unwrap_or_else(|_| panic!("lol"));
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
    let res: IResult<&'a str, &'a str, E> =
        peek(take_while1(|c: char| is_alphanumeric(c as u8)))(source);
    let (rest, prefix) = res.unwrap_or_else(|_| panic!("lol"));
    match prefix {
        "schema" | "type" | "interface" | "scalar" | "union" | "enum" | "input" | "directive" => {
            type_system_definition(rest)
                .map(|(rest, tsd)| (rest, Definition::TypeSystemDefinition(Box::new(tsd))))
        }
        _ => Err(nom::Err::Failure(error_position!(
            source,
            ErrorKind::TagBits
        ))),
    }
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
        map(separated_nonempty_list(sp1, definition), |definitions| {
            Document {
                definitions,
                loc: None,
            }
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
