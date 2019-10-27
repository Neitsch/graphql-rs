use super::ast::*;
use super::source::Source;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while, take_while1},
    character::{complete::one_of, is_alphanumeric, is_digit},
    combinator::{map, opt, recognize},
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
pub fn parse<'a>(source: &'a Source) -> Document<'a> {
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
pub fn parse_value<'a>(source: &'a Source) -> Value<'a> {
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
pub fn parse_type<'a>(source: &'a Source) -> Type<'a> {
    let parse_result = type_node::<(&str, ErrorKind)>(source)(&source.body)
        .map_err(|e| panic!("{:?}", e))
        .unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

impl<'a> From<&'a Source> for Document<'a> {
    fn from(source: &'a Source) -> Document<'a> {
        parse(source)
    }
}

fn with_location<'a, O: WithLocation<'a> + Sized, E: ParseError<&'a str>, F>(
    f: F,
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    move |input: &'a str| {
        f(input).map(|mut result| {
            let loc = Location::new(
                source.body.len() - input.len(),
                source.body.len() - (&result.0).len(),
                source,
            );
            result.1.with_loc(loc);
            result
        })
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
) -> impl Fn(&'a str) -> IResult<&'a str, Name<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                take_while1(|c: char| is_alphanumeric_or_underscope(c as u8)),
                |string: &'a str| Name {
                    value: string.to_owned(),
                    loc: None,
                },
            ),
            source,
        )(input)
    }
}

fn description<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, StringValue<'a>, E> {
    move |input: &'a str| string_value(source)(input)
}

fn variable<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Variable<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(preceded(graphql_tag("$"), name(source)), |name| Variable {
                loc: None,
                name,
            }),
            source,
        )(input)
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
) -> impl Fn(&'a str) -> IResult<&'a str, FloatValue<'a>, E> {
    move |input: &'a str| {
        with_location(
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
            ),
            source,
        )(input)
    }
}

fn int_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, IntValue<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(recognize(integer_part), |val| IntValue {
                loc: None,
                value: val.to_string(),
            }),
            source,
        )(input)
    }
}

fn string_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, StringValue<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                alt((
                    delimited(
                        tag("\"\"\""),
                        move |input: &'a str| {
                            let mut cnt = 0;
                            let mut string_parts: Vec<&'a str> = vec![];
                            while input.len() - cnt > 0 {
                                if &input[cnt..]
                                    .chars()
                                    .take(3)
                                    .collect::<Vec<char>>()
                                    .iter()
                                    .collect::<String>()
                                    == "\"\"\""
                                {
                                    break;
                                } else if &input[cnt..]
                                    .chars()
                                    .take(4)
                                    .collect::<Vec<char>>()
                                    .iter()
                                    .collect::<String>()
                                    == "\\\"\"\""
                                {
                                    string_parts.push("\"\"\"");
                                    cnt += 4;
                                } else {
                                    let l = input[cnt..]
                                        .chars()
                                        .take(1)
                                        .collect::<Vec<char>>()
                                        .iter()
                                        .collect::<String>()
                                        .len();
                                    string_parts.push(&input[cnt..cnt + l]);
                                    cnt += l;
                                }
                            }
                            Ok((&input[cnt..], (Some(true), string_parts.join(""))))
                        },
                        tag("\"\"\""),
                    ),
                    delimited(
                        tag("\""),
                        move |input: &'a str| {
                            let mut cnt = 0;
                            let mut string_parts: Vec<&'a str> = vec![];
                            while input.len() - cnt > 0 {
                                if &input[cnt..]
                                    .chars()
                                    .take(1)
                                    .collect::<Vec<char>>()
                                    .iter()
                                    .collect::<String>()
                                    == "\""
                                {
                                    break;
                                } else if &input[cnt..]
                                    .chars()
                                    .take(1)
                                    .collect::<Vec<char>>()
                                    .iter()
                                    .collect::<String>()
                                    == "\\"
                                    && "\"/\\bfnrt".contains(
                                        &input[cnt..]
                                            .chars()
                                            .skip(1)
                                            .take(1)
                                            .collect::<Vec<char>>()
                                            .iter()
                                            .collect::<String>(),
                                    )
                                {
                                    let l = input[cnt..]
                                        .chars()
                                        .skip(1)
                                        .take(1)
                                        .collect::<Vec<char>>()
                                        .iter()
                                        .collect::<String>()
                                        .len();
                                    string_parts.push(&input[cnt + 1..cnt + l]);
                                    cnt += l;
                                } else {
                                    let l = input[cnt..]
                                        .chars()
                                        .take(1)
                                        .collect::<Vec<char>>()
                                        .iter()
                                        .collect::<String>()
                                        .len();
                                    string_parts.push(&input[cnt..cnt + l]);
                                    cnt += l;
                                }
                            }
                            Ok((&input[cnt..], (Some(false), string_parts.join(""))))
                        },
                        tag("\""),
                    ),
                )),
                |(block, value)| StringValue {
                    value,
                    loc: None,
                    block,
                },
            ),
            source,
        )(input)
    }
}

fn boolean_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, BooleanValue<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                alt((map(tag("true"), |_| true), map(tag("false"), |_| false))),
                |b| BooleanValue {
                    value: b,
                    loc: None,
                },
            ),
            source,
        )(input)
    }
}

fn null_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, NullValue<'a>, E> {
    move |input: &'a str| {
        with_location(map(tag("NULL"), |_| NullValue { loc: None }), source)(input)
    }
}

fn enum_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, EnumValue<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(name(source), |v| EnumValue {
                value: v.value,
                loc: None,
            }),
            source,
        )(input)
    }
}

fn list_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ListValue<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                delimited(
                    graphql_tag("["),
                    many0(preceded(opt(sp1), value(source))),
                    graphql_tag("]"),
                ),
                |v| ListValue {
                    values: v.into(),
                    loc: None,
                },
            ),
            source,
        )(input)
    }
}

fn object_field<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ObjectField<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                separated_pair(name(source), graphql_tag(":"), value(source)),
                |(name, value)| ObjectField {
                    loc: None,
                    value,
                    name,
                },
            ),
            source,
        )(input)
    }
}

fn object_value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ObjectValue<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                delimited(
                    graphql_tag("{"),
                    many0(preceded(opt(sp1), object_field(source))),
                    graphql_tag("}"),
                ),
                |v| ObjectValue {
                    fields: v.into(),
                    loc: None,
                },
            ),
            source,
        )(input)
    }
}

fn value<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Value<'a>, E> {
    move |input: &'a str| {
        alt((
            map(variable(source), |v| Value::Variable(v)),
            map(float_value(source), |v| Value::FloatValue(v)),
            map(int_value(source), |v| Value::IntValue(v)),
            map(string_value(source), |v| Value::StringValue(v)),
            map(boolean_value(source), |v| Value::BooleanValue(v)),
            map(null_value(source), |v| Value::NullValue(v)),
            map(enum_value(source), |v| Value::EnumValue(v)),
            map(list_value(source), |v| Value::ListValue(v)),
            map(object_value(source), |v| Value::ObjectValue(v)),
        ))(input)
    }
}

fn argument<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Argument<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                separated_pair(name(source), graphql_tag(":"), value(source)),
                |(name, value)| Argument {
                    loc: None,
                    name,
                    value,
                },
            ),
            source,
        )(input)
    }
}

fn arguments<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<Argument<'a>>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, Directive<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                preceded(graphql_tag("@"), pair(name(source), opt(arguments(source)))),
                |(name, args)| Directive {
                    loc: None,
                    name,
                    arguments: args.into(),
                },
            ),
            source,
        )(input)
    }
}

fn directives<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<Directive<'a>>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, NamedType<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(name(source), |name| NamedType { loc: None, name }),
            source,
        )(input)
    }
}

fn root_operation_type_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, OperationTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                separated_pair(operation_type, graphql_tag(":"), named_type(source)),
                |(operation, named_type)| OperationTypeDefinition {
                    loc: None,
                    operation,
                    _type: named_type,
                },
            ),
            source,
        )(input)
    }
}

fn schema_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, SchemaDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
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
                ),
                |(directives, operation_types)| SchemaDefinition {
                    loc: None,
                    directives: directives.into(),
                    operation_types: operation_types.into(),
                },
            ),
            source,
        )(input)
    }
}

fn directive_locations<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<Name<'a>>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, Value<'a>, E> {
    move |input: &'a str| preceded(graphql_tag("="), value(source))(input)
}

fn list_type<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ListType<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                preceded(
                    graphql_tag("["),
                    terminated(type_node(source), graphql_tag("]")),
                ),
                |v| ListType {
                    loc: None,
                    _type: Box::new(v),
                },
            ),
            source,
        )(input)
    }
}

fn non_null_type<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, NonNullType<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                terminated(
                    alt((
                        move |input: &'a str| {
                            map(list_type(source), |v| NonNullInnerType::ListType(v))(input)
                        },
                        move |input: &'a str| {
                            map(named_type(source), |v| NonNullInnerType::NamedType(v))(input)
                        },
                    )),
                    graphql_tag("!"),
                ),
                |v| NonNullType {
                    loc: None,
                    _type: v,
                },
            ),
            source,
        )(input)
    }
}

fn type_node<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Type<'a>, E> {
    move |input: &'a str| {
        alt((
            map(non_null_type(source), |v| Type::NonNullType(v)),
            map(list_type(source), |v| Type::ListType(v)),
            map(named_type(source), |v| Type::NamedType(v)),
        ))(input)
    }
}

fn input_value_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, InputValueDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    terminated(name(source), graphql_tag(":")),
                    type_node(source),
                    opt(default_value(source)),
                    opt(directives(source)),
                )),
                |(desc, name, type_node, default_value, directives)| InputValueDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    _type: type_node,
                    default_value: default_value.into(),
                    directives: directives.into(),
                },
            ),
            source,
        )(input)
    }
}

fn argument_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<InputValueDefinition<'a>>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, DirectiveDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    preceded(
                        graphql_tag("directive"),
                        preceded(graphql_tag("@"), name(source)),
                    ),
                    terminated(opt(argument_definition(source)), tag("on")),
                    directive_locations(source),
                )),
                |(desc, name, arguments, locations)| DirectiveDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    arguments: arguments.into(),
                    locations: locations.into(),
                },
            ),
            source,
        )(input)
    }
}

fn union_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, UnionTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
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
                ),
                |(desc, name, directives, union_member_types)| UnionTypeDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                    types: union_member_types.into(),
                },
            ),
            source,
        )(input)
    }
}

fn scalar_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ScalarTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    preceded(graphql_tag("scalar"), name(source)),
                    terminated(opt(directives(source)), opt(sp1)),
                )),
                |(desc, name, directives)| ScalarTypeDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                },
            ),
            source,
        )(input)
    }
}

fn field_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, FieldDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    name(source),
                    opt(argument_definition(source)),
                    preceded(graphql_tag(":"), type_node(source)),
                    opt(directives(source)),
                )),
                |(desc, name, argument_definition, type_node, directives)| FieldDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    arguments: argument_definition.into(),
                    _type: type_node,
                    directives: directives.into(),
                },
            ),
            source,
        )(input)
    }
}

fn fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<FieldDefinition<'a>>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, InterfaceTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    preceded(graphql_tag("interface"), name(source)),
                    opt(directives(source)),
                    opt(fields_definition(source)),
                )),
                |(desc, name, directives, fd)| InterfaceTypeDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                    fields: fd.into(),
                },
            ),
            source,
        )(input)
    }
}

fn input_fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, Vec<InputValueDefinition<'a>>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, InputObjectTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    preceded(graphql_tag("input"), name(source)),
                    opt(directives(source)),
                    opt(input_fields_definition(source)),
                )),
                |(desc, name, directives, ifd)| InputObjectTypeDefinition {
                    loc: None,
                    description: desc.into(),
                    name,
                    directives: directives.into(),
                    fields: ifd.into(),
                },
            ),
            source,
        )(input)
    }
}

fn object_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, ObjectTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
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
                )),
                |(desc, name_value, implements_interfaces, directives, fd)| ObjectTypeDefinition {
                    loc: None,
                    description: desc.into(),
                    name: name_value,
                    interfaces: implements_interfaces.into(),
                    directives: directives.into(),
                    fields: fd.into(),
                },
            ),
            source,
        )(input)
    }
}

fn type_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, TypeDefinition<'a>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, EnumTypeDefinition<'a>, E> {
    move |input: &'a str| {
        with_location(
            map(
                tuple((
                    opt(terminated(description(source), opt(sp1))),
                    preceded(graphql_tag("enum"), name(source)),
                    opt(directives(source)),
                    opt(delimited(
                        graphql_tag("{"),
                        many0(move |input: &'a str| {
                            with_location(
                                map(
                                    terminated(
                                        tuple((
                                            opt(terminated(description(source), opt(sp1))),
                                            name(source),
                                            opt(directives(source)),
                                        )),
                                        opt(sp1),
                                    ),
                                    |(desc, n, d)| EnumValueDefinition {
                                        loc: None,
                                        description: desc.into(),
                                        name: n,
                                        directives: d.into(),
                                    },
                                ),
                                source,
                            )(input)
                        }),
                        graphql_tag("}"),
                    )),
                )),
                |(desc, name_value, directives_value, values)| EnumTypeDefinition {
                    loc: None,
                    description: desc.into(),
                    name: name_value,
                    directives: directives_value.into(),
                    values: values.into(),
                },
            ),
            source,
        )(input)
    }
}

fn type_system_definition<'a, E: ParseError<&'a str>>(
    source: &'a Source,
) -> impl Fn(&'a str) -> IResult<&'a str, TypeSystemDefinition<'a>, E> {
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
) -> impl Fn(&'a str) -> IResult<&'a str, Definition<'a>, E> {
    move |input: &'a str| map(type_system_definition(source), std::convert::Into::into)(input)
}

fn document<'a, E: ParseError<&'a str>>(source: &'a Source) -> IResult<&'a str, Document<'a>, E> {
    let document_parser = with_location(
        map(many0(definition(source)), |definitions| Document {
            definitions: definitions.into(),
            loc: None,
        }),
        source,
    );
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
    fn test_string_value_5() {
        let source = Source::new("\" He said: \\\"Hello\\\" \" ".to_owned(), None, None);
        assert_json_snapshot_matches!(string_value::<(&str, ErrorKind)>(&source)(&source.body)
            .unwrap_or_else(|_| panic!("Test failed")));
    }

    #[test]
    fn test_string_value_6() {
        let source = Source::new("\"\"\" He said: \"Hello\" \"\"\" ".to_owned(), None, None);
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
