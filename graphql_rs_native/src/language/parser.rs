use super::ast::*;
use super::source::Source;
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_while, take_while1},
    character::{complete::one_of, is_alphanumeric, is_digit},
    combinator::{map, not, opt, recognize},
    error::{ErrorKind, ParseError},
    multi::{many0, many1, separated_list},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

/// Takes a graphql source representation and returns the parsed document.
/// ```
/// //# use graphql_rs_native::language::parser::parse;
/// //# use graphql_rs_native::language::source::{Source, Location as LocationOffset};
/// //# use graphql_rs_native::language::ast::{
/// //#   Document, DefinitionVec, Definition, TypeSystemDefinition, TypeDefinition, NamedType,
/// //#   Description, Name, OptDirectiveVec, OptFieldDefinitionVec, ObjectTypeDefinition,
/// //#   OptInputValueDefinitionVec, FieldDefinition, FieldDefinitionVec, Type, Location
/// //# };
/// //let source = Source::new("type User { id: ID }".to_string(), None, None);
/// //let document = parse(&source);
/// //assert_eq!(document, Document {
/// //   loc: Some(Location {
/// //       start: 0,
/// //       end: 20,
/// //       source: Source::new(
/// //           source.body.clone(),
/// //           Some("GraphQL Request".to_owned()),
/// //           Some(LocationOffset::new(1, 1))
/// //       )
/// //   }),
/// //   definitions: vec![
/// //        TypeSystemDefinition::from(TypeDefinition::from(ObjectTypeDefinition{
/// //            loc: Some(Location {
/// //                start: 0,
/// //                end: 20,
/// //                source: Source::new(
/// //                    source.body.clone(),
/// //                    Some("GraphQL Request".to_owned()),
/// //                    Some(LocationOffset::new(1, 1))
/// //                )
/// //            }),
/// //            description: None.into(),
/// //            name: Name::new("User".to_string(), Location::new(0,0,&source)),
/// //            interfaces: None,
/// //            directives: None.into(),
/// //            fields: vec![FieldDefinition {
/// //                 loc: Some(Location {
/// //                     start: 12,
/// //                     end: 18,
/// //                     source: Source::new(
/// //                         source.body.clone(),
/// //                         Some("GraphQL Request".to_owned()),
/// //                         Some(LocationOffset::new(1, 1))
/// //                     )
/// //                 }),
/// //                 description: None.into(),
/// //                 name: Name::new("id".into(), Location::new(0,0,&source)),
/// //                 arguments: None.into(),
/// //                 _type: NamedType::from(Name::new("ID".into(), Location::new(0,0,&source))).into(),
/// //                 directives: None.into()
/// //             }].into(),
/// //         })).into()
/// //    ].into()
/// // });
/// ```
pub fn parse(source: &Source) -> Document {
    let parse_result = document::<(&str, ErrorKind)>(&source)
        .map_err(|e| panic!("{:?}", e))
        .unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

/// Takes a graphql source representation and returns the parsed value node.
/// ```
/// // # use graphql_rs_native::language::parser::parse_value;
/// // # use graphql_rs_native::language::source::Source;
/// // # use graphql_rs_native::language::ast::{NullValue, Value};
/// // let value_result = parse_value(&Source::new("NULL".to_string(), None, None));
/// // assert_eq!(value_result, Value::NullValue(Box::new(NullValue::default())));
/// ```
pub fn parse_value(source: &Source) -> Value {
    let parse_result = value::<(&str, ErrorKind)>(source)(&source.body)
        .map_err(|e| panic!("{:?}", e))
        .unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

/// Takes a graphql source representation and returns the parsed type node.
/// ```
/// // # use graphql_rs_native::language::parser::parse_type;
/// // # use graphql_rs_native::language::source::Source;
/// // # use graphql_rs_native::language::ast::{Type, NamedType, Name};
/// // let value_result = parse_type(&Source::new("Test".to_string(), None, None));
/// // assert_eq!(value_result, NamedType::from(Name::new("Test".into(), Location::new(0,0,&source))).into());
/// ```
pub fn parse_type(source: &Source) -> Type {
    let parse_result = type_node::<(&str, ErrorKind)>(source)(&source.body)
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
        parse(&source)
    }
}

fn is_alphanumeric_or_underscope(c: u8) -> bool {
    is_alphanumeric(c) || c == b'_'
}

/// Consumes any form of whitespace until a different character occurs.
fn sp1<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many1(alt((
        map(take_while1(move |c| " \n\r\t,".contains(c)), |_| ()),
        map(
            pair(
                take_while1(move |c| "#".contains(c)),
                take_till(move |c| "\n\r".contains(c)),
            ),
            |_| (),
        ),
    ))))(i)
}

/// A special tag for lexical tokens with insignificant whitespaces
fn graphql_tag<'a, E: ParseError<&'a str>>(
    t: &'a str,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E> {
    move |input: &'a str| delimited(opt(sp1), tag(t), opt(sp1))(input)
}

fn name<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Name, E> {
    move |input: &'a str| {
        take_while1(|c: char| is_alphanumeric_or_underscope(c as u8))(input).map(
            |(rest, string)| {
                (
                    rest,
                    Name::new(
                        string.to_string(),
                        Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        ),
                    ),
                )
            },
        )
    }
}

fn description<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, StringValue, E> {
    move |input: &'a str| string_value(source)(input)
}

fn variable<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Variable, E> {
    move |input: &'a str| {
        preceded(graphql_tag("$"), name(source))(input).map(|(rest, name)| {
            (
                rest,
                Variable {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    name,
                },
            )
        })
    }
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

fn float_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, FloatValue, E> {
    move |input: &'a str| {
        recognize(preceded(
            integer_part,
            alt((
                fractional_part,
                exponential_part,
                recognize(pair(fractional_part, exponential_part)),
            )),
        ))(input)
        .map(|(rest, res)| {
            (
                rest,
                FloatValue {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    value: res.to_string(),
                },
            )
        })
    }
}

fn int_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, IntValue, E> {
    move |input: &'a str| {
        recognize(integer_part)(input).map(|(rest, val)| {
            (
                rest,
                IntValue {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    value: val.to_string(),
                },
            )
        })
    }
}

fn string_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, StringValue, E> {
    move |input: &'a str| {
        alt((
            delimited(
                tag("\"\"\""),
                recognize(terminated(
                    many0(terminated(take(1usize), not(tag("\"\"\"")))),
                    take(1usize),
                )),
                tag("\"\"\""),
            ),
            delimited(tag("\""), take_till(|c| c == '"'), tag("\"")),
        ))(input)
        .map(|(rest, value)| {
            (
                rest,
                StringValue {
                    value: value.to_string(),
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    block: None,
                },
            )
        })
    }
}

fn boolean_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, BooleanValue, E> {
    move |input: &'a str| {
        alt((map(tag("true"), |_| true), map(tag("false"), |_| false)))(input).map(|(rest, b)| {
            (
                rest,
                BooleanValue {
                    value: b,
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                },
            )
        })
    }
}

fn null_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, NullValue, E> {
    move |input: &'a str| {
        tag("NULL")(input).map(|(rest, _)| {
            (
                rest,
                NullValue {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                },
            )
        })
    }
}

fn enum_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, EnumValue, E> {
    move |input: &'a str| {
        name(source)(input).map(|(rest, v)| {
            (
                rest,
                EnumValue {
                    value: v.value,
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                },
            )
        })
    }
}

fn list_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ListValue, E> {
    move |input: &'a str| {
        delimited(
            graphql_tag("["),
            many0(preceded(opt(sp1), value(source))),
            graphql_tag("]"),
        )(input)
        .map(|(rest, v)| {
            (
                rest,
                ListValue {
                    values: v.into(),
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                },
            )
        })
    }
}

fn object_field<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ObjectField, E> {
    move |input: &'a str| {
        separated_pair(name(source), graphql_tag(":"), value(source))(input).map(
            |(rest, (name, value))| {
                (
                    rest,
                    ObjectField {
                        loc: Some(Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        )),
                        value,
                        name,
                    },
                )
            },
        )
    }
}

fn object_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ObjectValue, E> {
    move |input: &'a str| {
        delimited(
            graphql_tag("{"),
            many0(preceded(opt(sp1), object_field(source))),
            graphql_tag("}"),
        )(input)
        .map(|(rest, v)| {
            (
                rest,
                ObjectValue {
                    fields: v.into(),
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                },
            )
        })
    }
}

fn value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Value, E> {
    move |input: &'a str| {
        alt((
            map(variable(source), |v| Value::Variable(Box::new(v))),
            map(float_value(source), |v| Value::FloatValue(Box::new(v))),
            map(int_value(source), |v| Value::IntValue(Box::new(v))),
            map(string_value(source), |v| Value::StringValue(Box::new(v))),
            map(boolean_value(source), |v| Value::BooleanValue(Box::new(v))),
            map(null_value(source), |v| Value::NullValue(Box::new(v))),
            map(enum_value(source), |v| Value::EnumValue(Box::new(v))),
            map(list_value(source), |v| Value::ListValue(Box::new(v))),
            map(object_value(source), |v| Value::ObjectValue(Box::new(v))),
        ))(input)
    }
}

fn argument<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Argument, E> {
    move |input: &'a str| {
        separated_pair(name(source), graphql_tag(":"), value(source))(input).map(
            |(rest, (name, value))| {
                (
                    rest,
                    Argument {
                        loc: Some(Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        )),
                        name,
                        value,
                    },
                )
            },
        )
    }
}

fn arguments<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<Argument>, E> {
    move |input: &'a str| {
        preceded(
            graphql_tag("("),
            terminated(
                many0(preceded(opt(sp1), argument(source))),
                graphql_tag(")"),
            ),
        )(input)
    }
}

fn directive<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Directive, E> {
    move |input: &'a str| {
        preceded(graphql_tag("@"), pair(name(source), opt(arguments(source))))(input).map(
            |(rest, (name, args))| {
                (
                    rest,
                    Directive {
                        loc: Some(Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        )),
                        name,
                        arguments: args.into(),
                    },
                )
            },
        )
    }
}

fn directives<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<Directive>, E> {
    move |input: &'a str| many1(directive(source))(input)
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

fn named_type<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, NamedType, E> {
    move |input: &'a str| {
        name(source)(input).map(|(rest, name)| {
            (
                rest,
                NamedType {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    name,
                },
            )
        })
    }
}

fn root_operation_type_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, OperationTypeDefinition, E> {
    move |input: &'a str| {
        separated_pair(operation_type, graphql_tag(":"), named_type(source))(input).map(
            |(rest, (operation, named_type))| {
                (
                    rest,
                    OperationTypeDefinition {
                        loc: Some(Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        )),
                        operation,
                        _type: named_type,
                    },
                )
            },
        )
    }
}

fn schema_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, SchemaDefinition, E> {
    move |input: &'a str| {
        preceded(
            tag("schema"),
            pair(
                opt(directives(source)),
                preceded(
                    graphql_tag("{"),
                    terminated(
                        many0(root_operation_type_definition(source)),
                        graphql_tag("}"),
                    ),
                ),
            ),
        )(input)
        .map(|(rest, (directives, operation_types))| {
            (
                rest,
                SchemaDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    directives: directives.into(),
                    operation_types: operation_types.into(),
                },
            )
        })
    }
}

fn directive_locations<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<Name>, E> {
    move |input_outer: &'a str| {
        preceded(
            opt(graphql_tag("|")),
            separated_list(graphql_tag("|"), move |input: &'a str| {
                let (rest, name_val) = name(source)(input)?;
                match name_val.value.as_ref() {
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
                    | "INPUT_FIELD_DEFINITION" => Ok((rest, name_val)),
                    _ => panic!("lol1"),
                }
            }),
        )(input_outer)
    }
}

fn default_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Value, E> {
    move |input: &'a str| preceded(graphql_tag("="), value(source))(input)
}

fn list_type<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ListType, E> {
    move |input: &'a str| {
        preceded(
            graphql_tag("["),
            terminated(type_node(source), graphql_tag("]")),
        )(input)
        .map(|(rest, v)| {
            (
                rest,
                ListType {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    _type: Box::new(v),
                },
            )
        })
    }
}

fn non_null_type<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, NonNullType, E> {
    move |input: &'a str| {
        terminated(
            alt((
                move |input: &'a str| {
                    list_type(source)(input)
                        .map(|(rest, v)| (rest, NonNullInnerType::ListType(Box::new(v))))
                },
                move |input: &'a str| {
                    named_type(source)(input)
                        .map(|(rest, v)| (rest, NonNullInnerType::NamedType(Box::new(v))))
                },
            )),
            graphql_tag("!"),
        )(input)
        .map(|(rest, v)| {
            (
                rest,
                NonNullType {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    _type: v,
                },
            )
        })
    }
}

fn type_node<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Type, E> {
    move |input: &'a str| {
        alt((
            map(non_null_type(source), |v| Type::NonNullType(Box::new(v))),
            map(list_type(source), |v| Type::ListType(Box::new(v))),
            map(named_type(source), |v| Type::NamedType(Box::new(v))),
        ))(input)
    }
}

fn input_value_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, InputValueDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            terminated(name(source), graphql_tag(":")),
            type_node(source),
            opt(default_value(source)),
            opt(directives(source)),
        ))(input)
        .map(
            |(rest, (desc, name, type_node, default_value, directives))| {
                (
                    rest,
                    InputValueDefinition {
                        loc: Some(Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        )),
                        description: desc.into(),
                        name,
                        _type: type_node,
                        default_value: default_value.into(),
                        directives: directives.into(),
                    },
                )
            },
        )
    }
}

fn argument_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<InputValueDefinition>, E> {
    move |input: &'a str| {
        preceded(
            graphql_tag("("),
            terminated(
                many1(preceded(opt(sp1), input_value_definition(source))),
                graphql_tag(")"),
            ),
        )(input)
    }
}

fn directive_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, DirectiveDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            preceded(
                graphql_tag("directive"),
                preceded(graphql_tag("@"), name(source)),
            ),
            terminated(opt(argument_definition(source)), tag("on")),
            directive_locations(source),
        ))(input)
        .map(|(rest, (desc, name, arguments, locations))| {
            (
                rest,
                DirectiveDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    description: desc.into(),
                    name,
                    arguments: arguments.into(),
                    locations,
                },
            )
        })
    }
}

fn union_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, UnionTypeDefinition, E> {
    move |input: &'a str| {
        terminated(
            tuple((
                opt(terminated(description(source), opt(sp1))),
                preceded(graphql_tag("union"), name(source)),
                opt(directives(source)),
                preceded(
                    graphql_tag("="),
                    opt(preceded(
                        preceded(opt(sp1), opt(graphql_tag("|"))),
                        separated_list(graphql_tag("|"), named_type(source)),
                    )),
                ),
            )),
            opt(sp1),
        )(input)
        .map(|(rest, (desc, name, directives, union_member_types))| {
            (
                rest,
                UnionTypeDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                    types: union_member_types,
                },
            )
        })
    }
}

fn scalar_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ScalarTypeDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            preceded(graphql_tag("scalar"), name(source)),
            terminated(opt(directives(source)), opt(sp1)),
        ))(input)
        .map(|(rest, (desc, name, directives))| {
            (
                rest,
                ScalarTypeDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                },
            )
        })
    }
}

fn field_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, FieldDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            name(source),
            opt(argument_definition(source)),
            preceded(graphql_tag(":"), type_node(source)),
            opt(directives(source)),
        ))(input)
        .map(
            |(rest, (desc, name, argument_definition, type_node, directives))| {
                (
                    rest,
                    FieldDefinition {
                        loc: Some(Location::new(
                            source.body.len() - input.len(),
                            source.body.len() - rest.len(),
                            source,
                        )),
                        description: desc.into(),
                        name,
                        arguments: argument_definition.into(),
                        _type: type_node,
                        directives: directives.into(),
                    },
                )
            },
        )
    }
}

fn fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<FieldDefinition>, E> {
    move |input: &'a str| {
        preceded(
            graphql_tag("{"),
            terminated(
                many0(preceded(opt(sp1), field_definition(source))),
                graphql_tag("}"),
            ),
        )(input)
    }
}

fn interface_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, InterfaceTypeDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            preceded(graphql_tag("interface"), name(source)),
            opt(directives(source)),
            opt(fields_definition(source)),
        ))(input)
        .map(|(rest, (desc, name, directives, fd))| {
            (
                rest,
                InterfaceTypeDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                    fields: fd.into(),
                },
            )
        })
    }
}

fn input_fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<InputValueDefinition>, E> {
    move |input: &'a str| {
        preceded(
            graphql_tag("{"),
            terminated(
                many0(preceded(opt(sp1), input_value_definition(source))),
                graphql_tag("}"),
            ),
        )(input)
    }
}

fn input_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, InputObjectTypeDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            preceded(graphql_tag("input"), name(source)),
            opt(directives(source)),
            opt(input_fields_definition(source)),
        ))(input)
        .map(|(rest, (desc, name, directives, ifd))| {
            (
                rest,
                InputObjectTypeDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                    fields: ifd.into(),
                },
            )
        })
    }
}

fn object_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ObjectTypeDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            preceded(graphql_tag("type"), name(source)),
            opt(preceded(
                graphql_tag("implements"),
                preceded(
                    opt(graphql_tag("&")),
                    separated_list(graphql_tag("&"), named_type(source)),
                ),
            )),
            opt(directives(source)),
            opt(fields_definition(source)),
        ))(input)
        .map(
            |(rest, (desc, name_value, implements_interfaces, directives, fd))| {
                (
                    rest,
                    ObjectTypeDefinition {
                        loc: Some(Location {
                            source: source.clone(),
                            start: (source.body.len() - input.len()) as u64,
                            end: (source.body.len() - rest.len()) as u64,
                        }),
                        description: desc.into(),
                        name: name_value,
                        interfaces: implements_interfaces,
                        directives: directives.into(),
                        fields: fd.into(),
                    },
                )
            },
        )
    }
}

fn type_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, TypeDefinition, E> {
    move |input: &'a str| {
        alt((
            map(object_definition(source), |graphql_object| {
                graphql_object.into()
            }),
            map(interface_definition(source), |graphql_object| {
                graphql_object.into()
            }),
            map(scalar_definition(source), |graphql_object| {
                graphql_object.into()
            }),
            map(union_definition(source), |graphql_object| {
                graphql_object.into()
            }),
            map(enum_definition(source), |graphql_object| {
                graphql_object.into()
            }),
            map(input_definition(source), |graphql_object| {
                graphql_object.into()
            }),
        ))(input)
    }
}

fn enum_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, EnumTypeDefinition, E> {
    move |input: &'a str| {
        tuple((
            opt(terminated(description(source), opt(sp1))),
            preceded(graphql_tag("enum"), name(source)),
            opt(directives(source)),
            opt(delimited(
                graphql_tag("{"),
                many0(move |input: &'a str| {
                    terminated(
                        tuple((
                            opt(terminated(description(source), opt(sp1))),
                            name(source),
                            opt(directives(source)),
                        )),
                        opt(sp1),
                    )(input)
                    .map(|(rest, (desc, n, d))| {
                        (
                            rest,
                            EnumValueDefinition {
                                loc: Some(Location::new(
                                    source.body.len() - input.len(),
                                    source.body.len() - rest.len(),
                                    source,
                                )),
                                description: desc.into(),
                                name: n,
                                directives: d.into(),
                            },
                        )
                    })
                }),
                graphql_tag("}"),
            )),
        ))(input)
        .map(|(rest, (desc, name_value, directives_value, values))| {
            (
                rest,
                EnumTypeDefinition {
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                    description: desc.into(),
                    name: name_value,
                    directives: directives_value.into(),
                    values: values.into(),
                },
            )
        })
    }
}

fn type_system_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, TypeSystemDefinition, E> {
    move |input: &'a str| {
        alt((
            map(schema_definition(source), std::convert::Into::into),
            map(directive_definition(source), std::convert::Into::into),
            map(type_definition(source), std::convert::Into::into),
        ))(input)
    }
}

fn definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Definition, E> {
    move |input: &'a str| map(type_system_definition(source), std::convert::Into::into)(input)
}

fn document<'a, E: ParseError<&'a str>>(source: &'a Source) -> IResult<&'a str, Document, E> {
    let document_parser = move |input: &'a str| {
        many0(definition(source))(input).map(|(rest, definitions)| {
            (
                rest,
                Document {
                    definitions: definitions.into(),
                    loc: Some(Location::new(
                        source.body.len() - input.len(),
                        source.body.len() - rest.len(),
                        source,
                    )),
                },
            )
        })
    };
    delimited(opt(sp1), document_parser, opt(sp1))(&source.body)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_json_snapshot_matches;

    #[test]
    fn test_fields_definition_x() {
        let source = Source::new("{f:S l:S }".to_owned(), None, None);
        assert_json_snapshot_matches!(
            fields_definition::<nom::error::VerboseError<&str>>(&source)(&source.body)
                .unwrap_or_else(|_| panic!("Test failed"))
        );
    }

    #[test]
    fn test_fields_definition() {
        let source = Source::new("{ id: ID }".to_owned(), None, None);
        assert_json_snapshot_matches!(fields_definition::<(&str, ErrorKind)>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_union_definition_1() {
        let source = Source::new("union MyUnion = MemberA | MemberB x".to_owned(), None, None);
        assert_json_snapshot_matches!(union_definition::<nom::error::VerboseError<&str>>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_schema_definition() {
        let source = Source::new("schema @abc {}".to_owned(), None, None);
        assert_json_snapshot_matches!(schema_definition::<(&str, ErrorKind)>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_type_system_definition() {
        let source = Source::new("schema {}".to_owned(), None, None);
        assert_json_snapshot_matches!(type_system_definition::<(&str, ErrorKind)>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_float_value() {
        let source = Source::new("1.1".to_owned(), None, None);
        assert_json_snapshot_matches!(float_value::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_string_value_1() {
        let source = Source::new("\"\" ".to_owned(), None, None);
        assert_json_snapshot_matches!(string_value::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_string_value_2() {
        let source = Source::new("\"My test text\" ".to_owned(), None, None);
        assert_json_snapshot_matches!(string_value::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_string_value_3() {
        let source = Source::new("\"\"\" \"\"\" ".to_owned(), None, None);
        assert_json_snapshot_matches!(string_value::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_string_value_4() {
        let source = Source::new("\"\"\"My test text\"\"\" ".to_owned(), None, None);
        assert_json_snapshot_matches!(string_value::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
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

    #[test]
    fn test_object_definition_1() {
        let source = Source::new("type User { id: ID }".to_owned(), None, None);
        assert_json_snapshot_matches!(object_definition::<(&str, ErrorKind)>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_object_definition_2() {
        let source = Source::new("type User".to_owned(), None, None);
        assert_json_snapshot_matches!(object_definition::<(&str, ErrorKind)>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_object_definition_3() {
        let source = Source::new(
            "\"\"\"Comment\"\"\"
            type User"
                .to_owned(),
            None,
            None,
        );
        assert_json_snapshot_matches!(object_definition::<(&str, ErrorKind)>(&source)(
            &source.body
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_type_definition_1() {
        let source = Source::new("input User".to_owned(), None, None);
        assert_json_snapshot_matches!(type_definition::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_definition() {
        let source = Source::new("schema {}".to_owned(), None, None);
        assert_json_snapshot_matches!(definition::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_document_1() {
        let source = Source::new("schema {}".to_owned(), None, None);
        assert_json_snapshot_matches!(document::<nom::error::VerboseError<&str>>(&source)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_document_2() {
        assert_json_snapshot_matches!(document::<(&str, ErrorKind)>(
            &Source::new("
    input TestInput {
        id: Int!
    }

    type Author {
        id: Int!
        firstName: String
        lastName: String
        posts: [Post]
    }

    \"\"\"
    Block comment
    \"\"\"
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
        orderBy: DeploymentOrder = { field: CREATED_AT direction: ASC }
        ): Post
    }

    \"\"\"
    Collaborators affiliation level with a subject.
    \"\"\"
    enum CollaboratorAffiliation {
      \"\"\"
      All collaborators the authenticated user can see.
      \"\"\"
      ALL

      \"\"\"
      All collaborators with permissions to an organization-owned subject, regardless of organization membership status.
      \"\"\"
      DIRECT

      \"\"\"
      All outside collaborators of an organization-owned subject.
      \"\"\"
      OUTSIDE
    }
            ".to_owned(), None, None)
        )
        .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_document_3() {
        assert_json_snapshot_matches!(document::<nom::error::VerboseError<&str>>(&Source::new(
            "type User".to_owned(),
            None,
            None
        ))
        .unwrap_or_else(|_| panic!("Test failed")));
    }
}
