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
/// # use graphql_rs_native::language::parser::parse;
/// # use graphql_rs_native::language::source::Source;
/// let document = parse(&Source::new("type User { id: ID }".to_string(), None, None));
/// assert_eq!(document.definitions.len(), 1);
/// ```
pub fn parse(source: &Source) -> Document {
    let parse_result = document::<(&str, ErrorKind)>(&source.body)
        .map_err(|e| panic!("{:?}", e))
        .unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

pub fn parse_value(source: &Source) -> Value {
    let parse_result = value::<(&str, ErrorKind)>(&source.body)
        .map_err(|e| panic!("{:?}", e))
        .unwrap();
    assert_eq!(parse_result.0.len(), 0);
    parse_result.1
}

pub fn parse_type(source: &Source) -> Type {
    let parse_result = type_node::<(&str, ErrorKind)>(&source.body)
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

fn description<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, StringValue, E> {
    string_value(source)
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

fn string_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, StringValue, E> {
    map(
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
        )),
        |value: &'a str| StringValue {
            value: value.to_string(),
            loc: None,
            block: None,
        },
    )(source)
}

fn boolean_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, BooleanValue, E> {
    map(
        alt((map(tag("true"), |_| true), map(tag("false"), |_| false))),
        |b| BooleanValue {
            value: b,
            loc: None,
        },
    )(source)
}

fn null_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, NullValue, E> {
    map(tag("NULL"), |_| NullValue { loc: None })(source)
}

fn enum_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, EnumValue, E> {
    map(name, |v| EnumValue {
        value: v.value,
        loc: None,
    })(source)
}

fn list_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, ListValue, E> {
    map(
        delimited(
            graphql_tag("["),
            many0(preceded(opt(sp1), value)),
            graphql_tag("]"),
        ),
        |v| ListValue {
            values: v.into(),
            loc: None,
        },
    )(source)
}

fn object_field<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, ObjectField, E> {
    map(
        separated_pair(name, graphql_tag(":"), value),
        |(name, value)| ObjectField {
            loc: None,
            value,
            name,
        },
    )(source)
}

fn object_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, ObjectValue, E> {
    map(
        delimited(
            graphql_tag("{"),
            many0(preceded(opt(sp1), object_field)),
            graphql_tag("}"),
        ),
        |v| ObjectValue {
            fields: v.into(),
            loc: None,
        },
    )(source)
}

#[test]
fn test_string_value_1() {
    assert_eq!(
        string_value::<(&str, ErrorKind)>("\"\" "),
        Ok((
            " ",
            StringValue {
                value: "".to_string(),
                block: None,
                loc: None,
            }
        ))
    );
}

#[test]
fn test_string_value_2() {
    assert_eq!(
        string_value::<(&str, ErrorKind)>("\"My test text\" "),
        Ok((
            " ",
            StringValue {
                value: "My test text".to_string(),
                block: None,
                loc: None,
            }
        ))
    );
}

#[test]
fn test_string_value_3() {
    assert_eq!(
        string_value::<(&str, ErrorKind)>("\"\"\" \"\"\" "),
        Ok((
            " ",
            StringValue {
                value: " ".to_string(),
                block: None,
                loc: None,
            }
        ))
    );
}

#[test]
fn test_string_value_4() {
    assert_eq!(
        string_value::<(&str, ErrorKind)>("\"\"\"My test text\"\"\" "),
        Ok((
            " ",
            StringValue {
                value: "My test text".to_string(),
                block: None,
                loc: None,
            }
        ))
    );
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
        map(string_value, |v| Value::StringValue(Box::new(v))),
        map(boolean_value, |v| Value::BooleanValue(Box::new(v))),
        map(null_value, |v| Value::NullValue(Box::new(v))),
        map(enum_value, |v| Value::EnumValue(Box::new(v))),
        map(list_value, |v| Value::ListValue(Box::new(v))),
        map(object_value, |v| Value::ObjectValue(Box::new(v))),
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
            name,
            arguments: args.into(),
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
            directives: directives.into(),
            operation_types: operation_types.into(),
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
                    arguments: None.into()
                }])
                .into(),
                operation_types: vec![].into()
            }
        ))
    );
}

fn directive_locations<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<Name>, E> {
    preceded(
        opt(graphql_tag("|")),
        separated_list(graphql_tag("|"), move |input: &'a str| {
            let (rest, name_val) = name(input)?;
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
    )(source)
}

fn default_value<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Value, E> {
    preceded(graphql_tag("="), value)(source)
}

fn list_type<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, ListType, E> {
    map(
        preceded(graphql_tag("["), terminated(type_node, graphql_tag("]"))),
        |v| ListType {
            loc: None,
            _type: Box::new(v),
        },
    )(source)
}

fn non_null_type<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, NonNullType, E> {
    map(
        terminated(
            alt((
                move |input: &'a str| {
                    list_type(input)
                        .map(|(rest, v)| (rest, NonNullInnerType::ListType(Box::new(v))))
                },
                move |input: &'a str| {
                    named_type(input)
                        .map(|(rest, v)| (rest, NonNullInnerType::NamedType(Box::new(v))))
                },
            )),
            graphql_tag("!"),
        ),
        |v| NonNullType {
            loc: None,
            _type: v,
        },
    )(source)
}

fn type_node<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Type, E> {
    alt((
        map(non_null_type, |v| Type::NonNullType(Box::new(v))),
        map(list_type, |v| Type::ListType(Box::new(v))),
        map(named_type, |v| Type::NamedType(Box::new(v))),
    ))(source)
}

fn input_value_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, InputValueDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            terminated(name, graphql_tag(":")),
            type_node,
            opt(default_value),
            opt(directives),
        )),
        |(desc, name, type_node, default_value, directives)| InputValueDefinition {
            loc: None,
            description: desc.into(),
            name,
            _type: type_node,
            default_value: default_value.into(),
            directives: directives.into(),
        },
    )(source)
}

fn argument_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<InputValueDefinition>, E> {
    preceded(
        graphql_tag("("),
        terminated(
            many1(preceded(opt(sp1), input_value_definition)),
            graphql_tag(")"),
        ),
    )(source)
}

fn directive_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, DirectiveDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            preceded(graphql_tag("directive"), preceded(graphql_tag("@"), name)),
            terminated(opt(argument_definition), tag("on")),
            directive_locations,
        )),
        |(desc, name, arguments, locations)| DirectiveDefinition {
            loc: None,
            description: desc.into(),
            name,
            arguments: arguments.into(),
            locations,
        },
    )(source)
}

fn union_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, UnionTypeDefinition, E> {
    map(
        terminated(
            tuple((
                opt(terminated(description, opt(sp1))),
                preceded(graphql_tag("union"), name),
                opt(directives),
                preceded(
                    graphql_tag("="),
                    opt(preceded(
                        preceded(opt(sp1), opt(graphql_tag("|"))),
                        separated_list(graphql_tag("|"), named_type),
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
            types: union_member_types,
        },
    )(source)
}

#[test]
fn test_union_definition_1() {
    assert_eq!(
        union_definition::<nom::error::VerboseError<&str>>("union MyUnion = MemberA | MemberB x"),
        Ok((
            "x",
            UnionTypeDefinition {
                description: None.into(),
                loc: None,
                name: Name {
                    value: "MyUnion".to_string(),
                    loc: None,
                },
                directives: None.into(),
                types: Some(vec![
                    NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "MemberA".to_string()
                        }
                    },
                    NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "MemberB".to_string()
                        }
                    }
                ])
            },
        ))
    )
}

fn scalar_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, ScalarTypeDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            preceded(graphql_tag("scalar"), name),
            terminated(opt(directives), opt(sp1)),
        )),
        |(desc, name, directives)| ScalarTypeDefinition {
            loc: None,
            description: desc.into(),
            name,
            directives: directives.into(),
        },
    )(source)
}

fn field_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, FieldDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            name,
            opt(argument_definition),
            preceded(graphql_tag(":"), type_node),
            opt(directives),
        )),
        |(desc, name, argument_definition, type_node, directives)| FieldDefinition {
            loc: None,
            description: desc.into(),
            name,
            arguments: argument_definition.into(),
            _type: type_node,
            directives: directives.into(),
        },
    )(source)
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

#[test]
fn test_fields_definition_x() {
    assert_eq!(
        fields_definition::<nom::error::VerboseError<&str>>("{f:S l:S }"),
        Ok((
            "",
            vec![
                FieldDefinition {
                    loc: None,
                    description: None.into(),
                    name: Name {
                        loc: None,
                        value: "f".to_string(),
                    },
                    arguments: None.into(),
                    _type: Type::NamedType(Box::new(NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "S".to_string(),
                        },
                    })),
                    directives: None.into(),
                },
                FieldDefinition {
                    loc: None,
                    description: None.into(),
                    name: Name {
                        loc: None,
                        value: "l".to_string(),
                    },
                    arguments: None.into(),
                    _type: Type::NamedType(Box::new(NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "S".to_string(),
                        },
                    })),
                    directives: None.into(),
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
                description: None.into(),
                name: Name {
                    loc: None,
                    value: "id".to_string()
                },
                arguments: None.into(),
                _type: Type::NamedType(Box::new(NamedType {
                    loc: None,
                    name: Name {
                        loc: None,
                        value: "ID".to_string()
                    }
                })),
                directives: None.into(),
            }]
        ))
    )
}

fn interface_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, InterfaceTypeDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            preceded(graphql_tag("interface"), name),
            opt(directives),
            opt(fields_definition),
        )),
        |(desc, name, directives, fd)| InterfaceTypeDefinition {
            loc: None,
            description: desc.into(),
            name,
            directives: directives.into(),
            fields: fd.into(),
        },
    )(source)
}

fn input_fields_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, Vec<InputValueDefinition>, E> {
    preceded(
        graphql_tag("{"),
        terminated(
            many0(preceded(opt(sp1), input_value_definition)),
            graphql_tag("}"),
        ),
    )(source)
}

fn input_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, InputObjectTypeDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            preceded(graphql_tag("input"), name),
            opt(directives),
            opt(input_fields_definition),
        )),
        |(desc, name, directives, ifd)| InputObjectTypeDefinition {
            loc: None,
            description: desc.into(),
            name,
            directives: directives.into(),
            fields: ifd.into(),
        },
    )(source)
}

fn object_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, ObjectTypeDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            preceded(graphql_tag("type"), name),
            opt(preceded(
                graphql_tag("implements"),
                preceded(
                    opt(graphql_tag("&")),
                    separated_list(graphql_tag("&"), named_type),
                ),
            )),
            opt(directives),
            opt(fields_definition),
        )),
        |(desc, name_value, implements_interfaces, directives, fd)| ObjectTypeDefinition {
            loc: None,
            description: desc.into(),
            name: name_value,
            interfaces: implements_interfaces,
            directives: directives.into(),
            fields: fd.into(),
        },
    )(source)
}

#[test]
fn test_object_definition_1() {
    assert_eq!(
        object_definition::<(&str, ErrorKind)>("type User { id: ID }"),
        Ok((
            "",
            ObjectTypeDefinition {
                loc: None,
                description: None.into(),
                name: Name {
                    loc: None,
                    value: "User".to_string(),
                },
                interfaces: None,
                directives: None.into(),
                fields: Some(vec![FieldDefinition {
                    loc: None,
                    description: None.into(),
                    name: Name {
                        loc: None,
                        value: "id".to_string()
                    },
                    arguments: None.into(),
                    _type: Type::NamedType(Box::new(NamedType {
                        loc: None,
                        name: Name {
                            loc: None,
                            value: "ID".to_string()
                        }
                    })),
                    directives: None.into(),
                }])
                .into()
            }
        ))
    );
}

#[test]
fn test_object_definition_2() {
    assert_eq!(
        object_definition::<(&str, ErrorKind)>("type User"),
        Ok((
            "",
            ObjectTypeDefinition {
                loc: None,
                description: None.into(),
                name: Name {
                    loc: None,
                    value: "User".to_string(),
                },
                interfaces: None,
                directives: None.into(),
                fields: None.into(),
            }
        ))
    )
}

#[test]
fn test_object_definition_3() {
    assert_eq!(
        object_definition::<(&str, ErrorKind)>(
            "\"\"\"Comment\"\"\"
        type User"
        ),
        Ok((
            "",
            ObjectTypeDefinition {
                loc: None,
                description: Some(StringValue {
                    value: "Comment".to_string(),
                    loc: None,
                    block: None
                })
                .into(),
                name: Name {
                    loc: None,
                    value: "User".to_string(),
                },
                interfaces: None,
                directives: None.into(),
                fields: None.into(),
            }
        ))
    )
}

fn type_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, TypeDefinition, E> {
    alt((
        map(object_definition, |graphql_object| {
            TypeDefinition::ObjectTypeDefinition(Box::new(graphql_object))
        }),
        map(interface_definition, |graphql_object| {
            TypeDefinition::InterfaceTypeDefinition(Box::new(graphql_object))
        }),
        map(scalar_definition, |graphql_object| {
            TypeDefinition::ScalarTypeDefinition(Box::new(graphql_object))
        }),
        map(union_definition, |graphql_object| {
            TypeDefinition::UnionTypeDefinition(Box::new(graphql_object))
        }),
        map(enum_definition, |graphql_object| {
            TypeDefinition::EnumTypeDefinition(Box::new(graphql_object))
        }),
        map(input_definition, |graphql_object| {
            TypeDefinition::InputObjectTypeDefinition(Box::new(graphql_object))
        }),
    ))(source)
}

#[test]
fn test_type_definition_1() {
    assert_eq!(
        type_definition::<(&str, ErrorKind)>("input User"),
        Ok((
            "",
            TypeDefinition::InputObjectTypeDefinition(Box::new(InputObjectTypeDefinition {
                loc: None,
                description: None.into(),
                name: Name {
                    value: "User".to_string(),
                    loc: None
                },
                directives: None.into(),
                fields: None.into(),
            }))
        ))
    )
}

fn enum_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, EnumTypeDefinition, E> {
    map(
        tuple((
            opt(terminated(description, opt(sp1))),
            preceded(graphql_tag("enum"), name),
            opt(directives),
            opt(delimited(
                graphql_tag("{"),
                many0(map(
                    terminated(
                        tuple((
                            opt(terminated(description, opt(sp1))),
                            name,
                            opt(directives),
                        )),
                        opt(sp1),
                    ),
                    |(desc, n, d)| EnumValueDefinition {
                        loc: None,
                        description: desc.into(),
                        name: n,
                        directives: d.into(),
                    },
                )),
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
    )(source)
}

fn type_system_definition<'a, E: ParseError<&'a str>>(
    source: &'a str,
) -> IResult<&'a str, TypeSystemDefinition, E> {
    alt((
        map(schema_definition, |sd| {
            TypeSystemDefinition::SchemaDefinition(Box::new(sd))
        }),
        map(directive_definition, |dd| {
            TypeSystemDefinition::DirectiveDefinition(Box::new(dd))
        }),
        map(type_definition, |td| {
            TypeSystemDefinition::TypeDefinition(Box::new(td))
        }),
    ))(source)
}

#[test]
fn test_type_system_definition() {
    assert_eq!(
        type_system_definition::<(&str, ErrorKind)>("schema {}"),
        Ok((
            "",
            TypeSystemDefinition::SchemaDefinition(Box::new(SchemaDefinition {
                loc: None,
                directives: None.into(),
                operation_types: vec![].into()
            }))
        ))
    );
}

fn definition<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Definition, E> {
    map(type_system_definition, |tsd| {
        Definition::TypeSystemDefinition(Box::new(tsd))
    })(source)
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
                    directives: None.into(),
                    operation_types: vec![].into()
                })
            )))
        ))
    );
}

fn document<'a, E: ParseError<&'a str>>(source: &'a str) -> IResult<&'a str, Document, E> {
    delimited(
        opt(sp1),
        map(many0(definition), |definitions| Document {
            definitions: definitions.into(),
            loc: None,
        }),
        opt(sp1),
    )(source)
}

#[test]
fn test_document_1() {
    assert_eq!(
        document::<nom::error::VerboseError<&str>>("schema {}"),
        Ok((
            "",
            Document {
                loc: None,
                definitions: vec![Definition::TypeSystemDefinition(Box::new(
                    TypeSystemDefinition::SchemaDefinition(Box::new(SchemaDefinition {
                        loc: None,
                        directives: None.into(),
                        operation_types: vec![].into()
                    }))
                ))]
                .into(),
            }
        ))
    );
}

#[test]
fn test_document_2() {
    use insta::assert_json_snapshot_matches;
    assert_json_snapshot_matches!(document::<(&str, ErrorKind)>(
        "
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
        "
    )
    .unwrap_or_else(|_| panic!("Test failed")));
}

#[test]
fn test_document_3() {
    use insta::assert_json_snapshot_matches;
    assert_json_snapshot_matches!(document::<nom::error::VerboseError<&str>>("type User")
        .unwrap_or_else(|_| panic!("Test failed")));
}
