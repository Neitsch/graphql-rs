use super::source::Source;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Location<'a> {
    /// The character offset at which this Node begins.
    pub start: u64,

    /// The character offset at which this Node ends.
    pub end: u64,

    /// The Source document the AST represents.
    #[serde(skip)]
    pub source: Option<&'a Source>,
}

impl<'a> Location<'a> {
    pub fn new(start: usize, end: usize, source: &'a Source) -> Location<'a> {
        Location {
            start: start as u64,
            end: end as u64,
            source: Some(source),
        }
    }
}

pub trait WithLocation<'a> {
    fn with_loc(&mut self, loc: Location<'a>);
}

pub trait Visitor {
    fn visit<'a, 'b>(&self, node: AST<'a, 'b>);
}

pub trait Visitable {
    fn visit<V: Visitor>(&self, visitor: &V);
}

impl Visitable for String {
    fn visit<V: Visitor>(&self, _visitor: &V) {}
}

impl Visitable for bool {
    fn visit<V: Visitor>(&self, _visitor: &V) {}
}

impl Visitable for Option<bool> {
    fn visit<V: Visitor>(&self, _visitor: &V) {}
}

macro_rules! ast_node_with_location {
    ( @ $(#[$sattr:meta])* $name:ident { } -> ($($result:tt)*) ) => (
        #[serde(tag = "kind")]
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        $(#[$sattr])*
        pub struct $name<'a> {
            pub loc: Option<Location<'a>>,
            $($result)*
        }

        impl <'a> WithLocation<'a> for $name<'a> {
            fn with_loc(&mut self, loc: Location<'a>) {
                self.loc = Some(loc);
            }
        }
    );

    ( @ $(#[$sattr:meta])* $name:ident { $(#[$attr:meta])* $param:ident : $type:ty, $($rest:tt)* } -> ($($result:tt)*) ) => (
        ast_node_with_location!(@ $(#[$sattr])* $name { $($rest)* } -> (
            $($result)*
            $(#[$attr])*
            pub $param : $type,
        ));
    );

    ( $(#[$sattr:meta])* $name:ident { $( $(#[$attr:meta])* $param:ident : $type:ty ),* $(,)* } ) => (
        ast_node_with_location!(@ $(#[$sattr])* $name { $($(#[$attr])* $param : $type,)* } -> ());

        impl <'a>Visitable for $name<'a> {
            fn visit<V: Visitor>(&self, visitor: &V) {
                visitor.visit(self.into());
                $(
                    self.$param.visit(visitor);
                )*
            }
        }
    );
}

macro_rules! wrapping_ast_node {
    ( $name:ident ( Option<$type:ty> )) => {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
        pub struct $name<'a>(pub Option<$type>);

        impl<'a> From<Option<$type>> for $name<'a> {
            fn from(d: Option<$type>) -> Self {
                $name(d)
            }
        }

        impl<'a> fmt::Display for $name<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if let $name(Some(name)) = &self {
                    write!(f, "{}", name)?;
                }
                Ok(())
            }
        }

        impl<'a> core::ops::Deref for $name<'a> {
            type Target = Option<$type>;

            fn deref(self: &'_ Self) -> &'_ Self::Target {
                &self.0
            }
        }

        impl<'a> core::ops::DerefMut for $name<'a> {
            fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
                &mut self.0
            }
        }

        impl<'a> Visitable for $name<'a> {
            fn visit<V: Visitor>(&self, visitor: &V) {
                if let $name(Some(v)) = self {
                    v.visit(visitor);
                }
            }
        }
    };

    ( $name:ident ( Vec<$type:ty> )) => {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub struct $name<'a>(pub Vec<$type>);

        impl<'a> From<Vec<$type>> for $name<'a> {
            fn from(d: Vec<$type>) -> Self {
                $name(d)
            }
        }

        impl<'a> core::ops::Deref for $name<'a> {
            type Target = Vec<$type>;

            fn deref(self: &'_ Self) -> &'_ Self::Target {
                &self.0
            }
        }

        impl<'a> core::ops::DerefMut for $name<'a> {
            fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
                &mut self.0
            }
        }

        impl<'a> Visitable for $name<'a> {
            fn visit<V: Visitor>(&self, visitor: &V) {
                for v in &self.0 {
                    v.visit(visitor);
                }
            }
        }
    };

    ( $name:ident ( $type:ty )) => {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub struct $name<'a>(pub $type);

        impl<'a> From<$type> for $name<'a> {
            fn from(d: $type) -> Self {
                $name(d)
            }
        }

        impl<'a> core::ops::Deref for $name<'a> {
            type Target = $type;

            fn deref(self: &'_ Self) -> &'_ Self::Target {
                &self.0
            }
        }

        impl<'a> core::ops::DerefMut for $name<'a> {
            fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
                &mut self.0
            }
        }

        impl<'a> Visitable for $name<'a> {
            fn visit<V: Visitor>(&self, visitor: &V) {
                self.0.visit(visitor)
            }
        }
    };
}

macro_rules! ast_enum {
    ( $type:ident, { $( $name:ident ), * } ) => {
        #[serde(tag = "kind")]
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub enum $type<'a> {
            $(
                $name($name<'a>),
            )+
        }

        impl<'a, 'b> From<&'b $type<'a>> for AST<'b, 'a> {
            fn from(v: &'b $type<'a>) -> Self {
                match v {
                    $(
                        $type::$name(x) => AST::from(x),
                    )+
                }
            }
        }

        impl <'a> fmt::Display for $type<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $type::$name(x) => write!(f, "{}", x),
                    )+
                }
            }
        }

        impl <'a> Visitable for $type<'a> {
            fn visit<V: Visitor>(&self, visitor: &V) {
                match self {
                    $(
                        $type::$name(v) => v.visit(visitor),
                    )+
                }
            }
        }

        $(
            impl <'a> From<$name<'a>> for $type<'a> {
                fn from(v: $name<'a>) -> Self {
                    $type::$name(v)
                }
            }
        )+
    };
}

macro_rules! ast_node_enum {
    ( $type:ident, { $( $name:ident ), * } ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum $type<'a,'b> {
            $(
                $name(&'a $name<'b>),
            )+
        }

        impl<'a, 'b> fmt::Display for $type<'a, 'b> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $type::$name(v) => write!(f, "{}", v),
                    )+
                }
            }
        }

        $(
            impl<'a, 'b> From<&'b $name<'a>> for $type<'b, 'a> {
                fn from(v: &'b $name<'a>) -> Self {
                    $type::$name(v)
                }
            }
        )+
    }
}

ast_node_enum!(AST, {
    Name,
    Document,
    OperationDefinition,
    VariableDefinition,
    Variable,
    SelectionSet,
    Field,
    Argument,
    FragmentSpread,
    InlineFragment,
    FragmentDefinition,
    IntValue,
    FloatValue,
    StringValue,
    BooleanValue,
    NullValue,
    EnumValue,
    ListValue,
    ObjectValue,
    ObjectField,
    Directive,
    NamedType,
    ListType,
    NonNullType,
    SchemaDefinition,
    OperationTypeDefinition,
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    FieldDefinition,
    InputValueDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    EnumValueDefinition,
    InputObjectTypeDefinition,
    DirectiveDefinition,
    SchemaExtension,
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension
});

// Name
ast_node_with_location!(Name { value: String });

impl<'a> Name<'a> {
    pub fn new(name: String, loc: Location<'a>) -> Name<'a> {
        Name {
            loc: Some(loc),
            value: name,
        }
    }
}

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

wrapping_ast_node!(OptName(Option<Name<'a>>));

wrapping_ast_node!(NameVec(Vec<Name<'a>>));

// Document
ast_node_with_location!(Document {
    definitions: DefinitionVec<'a>,
});

impl fmt::Display for Document<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.definitions,)
    }
}

ast_enum!(Definition, {
    ExecutableDefinition,
    TypeSystemDefinition,
    TypeSystemExtension
});

wrapping_ast_node!(DefinitionVec(Vec<Definition<'a>>));

impl<'a> fmt::Display for DefinitionVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|d| format!("{}", d))
                .collect::<Vec<String>>()
                .join("\n\n")
        )
    }
}

ast_enum!(ExecutableDefinition, {
    OperationDefinition,
    FragmentDefinition
});

ast_node_with_location!(OperationDefinition {
    operation: OperationType,
    name: OptName<'a>,
    #[serde(rename = "variableDefinitions")]
    variable_definitions: OptVariableDefinitionVec<'a>,
    directives: OptDirectiveVec<'a>,
    #[serde(rename = "selectionSet")]
    selection_set: SelectionSet<'a>
});

impl<'a> fmt::Display for OperationDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}{} {}",
            self.operation,
            self.name,
            self.variable_definitions,
            self.directives,
            self.selection_set,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum OperationType {
    #[serde(rename = "query")]
    QUERY,
    #[serde(rename = "mutation")]
    MUTATION,
    #[serde(rename = "subscription")]
    SUBSCRIPTION,
}

impl Visitable for OperationType {
    fn visit<V: Visitor>(&self, _v: &V) {}
}

impl fmt::Display for OperationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OperationType::QUERY => "query".to_string(),
                OperationType::MUTATION => "mutation".to_string(),
                OperationType::SUBSCRIPTION => "subscription".to_string(),
            }
        )
    }
}

ast_node_with_location!(VariableDefinition {
    variable: Variable<'a>,
    #[serde(rename = "type")]
    _type: Type<'a>,
    #[serde(rename = "defaultValue")]
    default_value: OptValue<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for VariableDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.variable, self._type)?;
        if let OptValue(Some(dv)) = &self.default_value {
            write!(f, " = {}", dv)?;
        }
        if let OptDirectiveVec(Some(d)) = &self.directives {
            write!(f, " {}", d)?;
        }
        Ok(())
    }
}

wrapping_ast_node!(VariableDefinitionVec(Vec<VariableDefinition<'a>>));

impl<'a> fmt::Display for VariableDefinitionVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|vd| format!("{}", vd))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

wrapping_ast_node!(OptVariableDefinitionVec(Option<VariableDefinitionVec<'a>>));

ast_node_with_location!(Variable { name: Name<'a> });

impl<'a> fmt::Display for Variable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name)
    }
}

ast_node_with_location!(SelectionSet {
    selections: SelectionVec<'a>
});

wrapping_ast_node!(OptSelectionSet(Option<SelectionSet<'a>>));

impl<'a> fmt::Display for SelectionSet<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{{}\n}}",
            self.selections
                .iter()
                .map(|v| format!("\n{}", v))
                .collect::<Vec<String>>()
                .join("")
                .replace("\n", "\n  "),
        )
    }
}

ast_enum!(Selection, {
    Field,
    FragmentSpread,
    InlineFragment
});

wrapping_ast_node!(SelectionVec(Vec<Selection<'a>>));

ast_node_with_location!(Field {
    alias: OptName<'a>,
    name: Name<'a>,
    arguments: OptArgumentVec<'a>,
    directives: OptDirectiveVec<'a>,
    #[serde(rename = "selectionSet")]
    selection_set: OptSelectionSet<'a>
});

impl<'a> fmt::Display for Field<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptName(Some(name)) = &self.alias {
            write!(f, "{}: ", name,)?;
        }
        write!(f, "{}{}{}", self.name, self.arguments, self.directives)?;
        if let OptSelectionSet(Some(sel)) = &self.selection_set {
            write!(f, " {}", sel)?;
        }
        Ok(())
    }
}

ast_node_with_location!(Argument {
    name: Name<'a>,
    value: Value<'a>
});

impl<'a> fmt::Display for Argument<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

wrapping_ast_node!(ArgumentVec(Vec<Argument<'a>>));

impl<'a> fmt::Display for ArgumentVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({})",
            self.iter()
                .map(|vd| format!("{}", vd))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

wrapping_ast_node!(OptArgumentVec(Option<ArgumentVec<'a>>));

impl<'a> From<Option<Vec<Argument<'a>>>> for OptArgumentVec<'a> {
    fn from(d: Option<Vec<Argument<'a>>>) -> Self {
        OptArgumentVec(d.map(std::convert::Into::into))
    }
}

// Fragments
ast_node_with_location!(FragmentSpread {
    name: Name<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for FragmentSpread<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...{}{}", self.name, self.directives)
    }
}

ast_node_with_location!(InlineFragment {
    #[serde(rename = "typeCondition")]
    type_condition: OptNamedType<'a>,
    directives: OptDirectiveVec<'a>,
    #[serde(rename = "selectionSet")]
    selection_set: SelectionSet<'a>
});

impl<'a> fmt::Display for InlineFragment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...")?;
        if let OptNamedType(Some(tc)) = &self.type_condition {
            write!(f, " on {}", tc)?;
        }
        write!(f, "{} {}", self.directives, self.selection_set)?;
        Ok(())
    }
}

ast_node_with_location!(FragmentDefinition {
    name: Name<'a>,
    #[serde(rename = "variableDefinitions")]
    variable_definitions: OptVariableDefinitionVec<'a>,
    #[serde(rename = "typeCondition")]
    type_condition: NamedType<'a>,
    directives: OptDirectiveVec<'a>,
    #[serde(rename = "selectionSet")]
    selection_set: SelectionSet<'a>,
});

impl<'a> fmt::Display for FragmentDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fragment {}{} on {} {}",
            self.name, self.variable_definitions, self.directives, self.selection_set,
        )
    }
}

// Values
ast_enum!(Value, {
    Variable,
    IntValue,
    FloatValue,
    StringValue,
    BooleanValue,
    NullValue,
    EnumValue,
    ListValue,
    ObjectValue
});

wrapping_ast_node!(OptValue(Option<Value<'a>>));

wrapping_ast_node!(ValueVec(Vec<Value<'a>>));

impl<'a> fmt::Display for ValueVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|vd| format!("{}", vd))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

ast_node_with_location!(IntValue { value: String });

impl<'a> fmt::Display for IntValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

ast_node_with_location!(FloatValue { value: String });

impl<'a> fmt::Display for FloatValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

ast_node_with_location!(StringValue {
    value: String,
    block: Option<bool>
});

impl<'a> fmt::Display for StringValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let delimiter = match &self.block {
            Some(true) => "\"\"\"",
            _ => "\"",
        };
        write!(f, "{}{}{}", delimiter, self.value, delimiter)
    }
}

wrapping_ast_node!(Description(Option<StringValue<'a>>));

ast_node_with_location!(BooleanValue { value: bool });

impl<'a> fmt::Display for BooleanValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            if self.value {
                "true".to_string()
            } else {
                "false".to_string()
            }
        )
    }
}

ast_node_with_location!(NullValue {});

impl<'a> fmt::Display for NullValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NULL")
    }
}

ast_node_with_location!(EnumValue { value: String });

impl<'a> fmt::Display for EnumValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

ast_node_with_location!(ListValue { values: ValueVec<'a> });

impl<'a> fmt::Display for ListValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.values)
    }
}

ast_node_with_location!(ObjectValue {
    fields: ObjectFieldVec<'a>
});

impl<'a> fmt::Display for ObjectValue<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fields)
    }
}

ast_node_with_location!(ObjectField {
    name: Name<'a>,
    value: Value<'a>
});

impl<'a> fmt::Display for ObjectField<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

wrapping_ast_node!(ObjectFieldVec(Vec<ObjectField<'a>>));

impl<'a> fmt::Display for ObjectFieldVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|vd| format!("{}", vd))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

// Directives
ast_node_with_location!(Directive {
    name: Name<'a>,
    arguments: OptArgumentVec<'a>,
});

impl<'a> fmt::Display for Directive<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)?;
        if let OptArgumentVec(Some(args)) = &self.arguments {
            write!(f, "{}", args)?;
        }
        Ok(())
    }
}

wrapping_ast_node!(DirectiveVec(Vec<Directive<'a>>));

impl<'a> fmt::Display for DirectiveVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|vd| format!("{}", vd))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

wrapping_ast_node!(OptDirectiveVec(Option<DirectiveVec<'a>>));

impl<'a> From<Option<Vec<Directive<'a>>>> for OptDirectiveVec<'a> {
    fn from(d: Option<Vec<Directive<'a>>>) -> Self {
        OptDirectiveVec(d.map(std::convert::Into::into))
    }
}

// Type Reference
ast_enum!(Type, {
    NamedType,
    ListType,
    NonNullType
});

ast_node_with_location!(NamedType { name: Name<'a> });

wrapping_ast_node!(NamedTypeVec(Vec<NamedType<'a>>));

impl<'a> fmt::Display for NamedTypeVec<'a> {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        panic!("Cannot format this");
    }
}

wrapping_ast_node!(OptNamedTypeVec(Option<NamedTypeVec<'a>>));

impl<'a> fmt::Display for NamedType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> From<Name<'a>> for NamedType<'a> {
    fn from(n: Name<'a>) -> Self {
        NamedType {
            loc: n.loc.clone(),
            name: n,
        }
    }
}

wrapping_ast_node!(OptNamedType(Option<NamedType<'a>>));

ast_node_with_location!(ListType {
    #[serde(rename = "type")]
    _type: Box<Type<'a>>,
});

impl<'a> fmt::Display for ListType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self._type)
    }
}

ast_enum!(NonNullInnerType, {
    NamedType,
    ListType
});

ast_node_with_location!(NonNullType {
    #[serde(rename = "type")]
    _type: NonNullInnerType<'a>
});

impl<'a> fmt::Display for NonNullType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}!", self._type)
    }
}

// Type System Definition
ast_enum!(TypeSystemDefinition, {
    SchemaDefinition,
    TypeDefinition,
    DirectiveDefinition
});

ast_node_with_location!(SchemaDefinition {
    directives: OptDirectiveVec<'a>,
    #[serde(rename = "operationTypes")]
    operation_types: OperationTypeDefinitionVec<'a>
});

impl<'a> fmt::Display for SchemaDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ot_formatter = format!("{}", self.operation_types);
        write!(
            f,
            "schema{} {{{}\n}}",
            self.directives,
            if ot_formatter.is_empty() {
                ot_formatter
            } else {
                ot_formatter.replace("\n", "\n  ")
            }
        )
    }
}

ast_node_with_location!(OperationTypeDefinition {
    operation: OperationType,
    #[serde(rename = "type")]
    _type: NamedType<'a>
});

impl<'a> fmt::Display for OperationTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.operation, self._type)
    }
}

wrapping_ast_node!(OperationTypeDefinitionVec(Vec<OperationTypeDefinition<'a>>));

impl<'a> fmt::Display for OperationTypeDefinitionVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|v| format!("\n{}", v))
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

wrapping_ast_node!(OptOperationTypeDefinitionVec(Option<OperationTypeDefinitionVec<'a>>));

impl<'a> From<Option<Vec<OperationTypeDefinition<'a>>>> for OptOperationTypeDefinitionVec<'a> {
    fn from(d: Option<Vec<OperationTypeDefinition<'a>>>) -> Self {
        OptOperationTypeDefinitionVec(d.map(std::convert::Into::into))
    }
}

// Type Definition
ast_enum!(TypeDefinition, {
    ScalarTypeDefinition,
    ObjectTypeDefinition,
    InterfaceTypeDefinition,
    UnionTypeDefinition,
    EnumTypeDefinition,
    InputObjectTypeDefinition
});

ast_node_with_location!(ScalarTypeDefinition {
    description: Description<'a>,
    name: Name<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for ScalarTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}scalar {}{}",
            self.description, self.name, self.directives
        )
    }
}

ast_node_with_location!(ObjectTypeDefinition {
    description: Description<'a>,
    name: Name<'a>,
    interfaces: InterfaceNamedTypeVec<'a>,
    directives: OptDirectiveVec<'a>,
    fields: OptFieldDefinitionVec<'a>
});

wrapping_ast_node!(InterfaceNamedTypeVec(OptNamedTypeVec<'a>));

impl<'a> From<Option<Vec<NamedType<'a>>>> for InterfaceNamedTypeVec<'a> {
    fn from(d: Option<Vec<NamedType<'a>>>) -> Self {
        InterfaceNamedTypeVec(OptNamedTypeVec::from(d))
    }
}

impl<'a> From<Option<Vec<NamedType<'a>>>> for OptNamedTypeVec<'a> {
    fn from(d: Option<Vec<NamedType<'a>>>) -> Self {
        OptNamedTypeVec(d.map(|v| v.into()))
    }
}

impl<'a> fmt::Display for ObjectTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}type {}{}{}{}",
            self.description, self.name, self.interfaces, self.directives, self.fields
        )
    }
}

impl<'a> fmt::Display for InterfaceNamedTypeVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let InterfaceNamedTypeVec(OptNamedTypeVec(Some(itf))) = self {
            write!(
                f,
                "{}",
                itf.iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<String>>()
                    .join(" & ")
            )
        } else {
            write!(f, "")
        }
    }
}

ast_node_with_location!(FieldDefinition {
    description: Description<'a>,
    name: Name<'a>,
    arguments: OptInputValueDefinitionVec<'a>,
    #[serde(rename = "type")]
    _type: Type<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for FieldDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}: {}{}",
            self.description, self.name, self.arguments, self._type, self.directives
        )
    }
}

wrapping_ast_node!(FieldDefinitionVec(Vec<FieldDefinition<'a>>));

impl<'a> fmt::Display for FieldDefinitionVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

wrapping_ast_node!(OptFieldDefinitionVec(Option<FieldDefinitionVec<'a>>));

impl<'a> From<Option<Vec<FieldDefinition<'a>>>> for OptFieldDefinitionVec<'a> {
    fn from(d: Option<Vec<FieldDefinition<'a>>>) -> Self {
        OptFieldDefinitionVec(d.map(std::convert::Into::into))
    }
}

impl<'a> From<Vec<FieldDefinition<'a>>> for OptFieldDefinitionVec<'a> {
    fn from(v: Vec<FieldDefinition<'a>>) -> Self {
        Some(v).into()
    }
}

ast_node_with_location!(InputValueDefinition {
    description: Description<'a>,
    name: Name<'a>,
    #[serde(rename = "type")]
    _type: Type<'a>,
    #[serde(rename = "defaultValue")]
    default_value: OptValue<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for InputValueDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}: {}{}{}",
            self.description, self.name, self._type, self.default_value, self.directives
        )
    }
}

wrapping_ast_node!(InputValueDefinitionVec(Vec<InputValueDefinition<'a>>));

impl<'a> fmt::Display for InputValueDefinitionVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

wrapping_ast_node!(OptInputValueDefinitionVec(Option<InputValueDefinitionVec<'a>>));

impl<'a> From<Option<Vec<InputValueDefinition<'a>>>> for OptInputValueDefinitionVec<'a> {
    fn from(d: Option<Vec<InputValueDefinition<'a>>>) -> Self {
        OptInputValueDefinitionVec(d.map(std::convert::Into::into))
    }
}

ast_node_with_location!(InterfaceTypeDefinition {
    description: Description<'a>,
    name: Name<'a>,
    directives: OptDirectiveVec<'a>,
    fields: OptFieldDefinitionVec<'a>
});

impl<'a> fmt::Display for InterfaceTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}interface {}{}{}",
            self.description, self.name, self.directives, self.fields
        )
    }
}

ast_node_with_location!(UnionTypeDefinition {
    description: Description<'a>,
    name: Name<'a>,
    directives: OptDirectiveVec<'a>,
    types: UnionNamedTypeVec<'a>
});

wrapping_ast_node!(UnionNamedTypeVec(OptNamedTypeVec<'a>));

impl<'a> From<Option<Vec<NamedType<'a>>>> for UnionNamedTypeVec<'a> {
    fn from(d: Option<Vec<NamedType<'a>>>) -> Self {
        UnionNamedTypeVec(OptNamedTypeVec::from(d))
    }
}

impl<'a> fmt::Display for UnionTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}union {}{} = {}",
            self.description, self.name, self.directives, self.types
        )
    }
}

impl<'a> fmt::Display for UnionNamedTypeVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let UnionNamedTypeVec(OptNamedTypeVec(Some(types))) = self {
            write!(
                f,
                "{}",
                types
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<String>>()
                    .join(" | ")
            )
        } else {
            write!(f, "")
        }
    }
}

ast_node_with_location!(EnumTypeDefinition {
    description: Description<'a>,
    name: Name<'a>,
    directives: OptDirectiveVec<'a>,
    values: OptEnumValueDefinitionVec<'a>
});

impl<'a> fmt::Display for EnumTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}enum {}{}{}",
            self.description, self.name, self.directives, self.values
        )
    }
}

ast_node_with_location!(EnumValueDefinition {
    description: Description<'a>,
    name: Name<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for EnumValueDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.description, self.name, self.directives)
    }
}

wrapping_ast_node!(EnumValueDefinitionVec(Vec<EnumValueDefinition<'a>>));

impl<'a> fmt::Display for EnumValueDefinitionVec<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

wrapping_ast_node!(OptEnumValueDefinitionVec(Option<EnumValueDefinitionVec<'a>>));

impl<'a> From<Option<Vec<EnumValueDefinition<'a>>>> for OptEnumValueDefinitionVec<'a> {
    fn from(d: Option<Vec<EnumValueDefinition<'a>>>) -> Self {
        OptEnumValueDefinitionVec(d.map(std::convert::Into::into))
    }
}

ast_node_with_location!(InputObjectTypeDefinition {
    description: Description<'a>,
    name: Name<'a>,
    directives: OptDirectiveVec<'a>,
    fields: OptInputValueDefinitionVec<'a>
});

impl<'a> fmt::Display for InputObjectTypeDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}input {}{}{}",
            self.description, self.name, self.directives, self.fields
        )
    }
}

// Directive Definitions
ast_node_with_location!(DirectiveDefinition {
    description: Description<'a>,
    name: Name<'a>,
    arguments: OptInputValueDefinitionVec<'a>,
    locations: NameVec<'a>
});

impl<'a> fmt::Display for DirectiveDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}directive @{}{} on {}",
            self.description,
            self.name,
            self.arguments,
            self.locations
                .iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join(" | ")
        )
    }
}

// Type System Extensions
ast_enum!(TypeSystemExtension, {
    SchemaExtension,
    TypeExtension
});

ast_node_with_location!(SchemaExtension {
    directives: OptDirectiveVec<'a>,
    #[serde(rename = "operationTypes")]
    operation_types: OptOperationTypeDefinitionVec<'a>
});

impl<'a> fmt::Display for SchemaExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend schema{}{}",
            self.directives, self.operation_types
        )
    }
}

// Type Extensions
ast_enum!(TypeExtension, {
    ScalarTypeExtension,
    ObjectTypeExtension,
    InterfaceTypeExtension,
    UnionTypeExtension,
    EnumTypeExtension,
    InputObjectTypeExtension
});

ast_node_with_location!(ScalarTypeExtension {
    name: Name<'a>,
    directives: OptDirectiveVec<'a>
});

impl<'a> fmt::Display for ScalarTypeExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extend scalar{}{}", self.name, self.directives)
    }
}

ast_node_with_location!(ObjectTypeExtension {
    name: Name<'a>,
    interfaces: InterfaceNamedTypeVec<'a>,
    directives: OptDirectiveVec<'a>,
    fields: OptFieldDefinitionVec<'a>
});

impl<'a> fmt::Display for ObjectTypeExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend type {}{}{}{}",
            self.name, self.interfaces, self.directives, self.fields
        )
    }
}

ast_node_with_location!(InterfaceTypeExtension {
    name: Name<'a>,
    directives: OptDirectiveVec<'a>,
    fields: OptFieldDefinitionVec<'a>
});

impl<'a> fmt::Display for InterfaceTypeExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend interface {}{}{}",
            self.name, self.directives, self.fields
        )
    }
}

ast_node_with_location!(UnionTypeExtension {
    name: Name<'a>,
    directives: OptDirectiveVec<'a>,
    types: UnionNamedTypeVec<'a>
});

impl<'a> fmt::Display for UnionTypeExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend union {}{} = {}",
            self.name, self.directives, self.types
        )
    }
}

ast_node_with_location!(EnumTypeExtension {
    #[doc = "The name of the \"target\" enum"]
    name: Name<'a>,
    #[doc = "A list of directives assigned to the enum extension"]
    directives: OptDirectiveVec<'a>,
    #[doc = "A list of possible values to extend the enum by."]
    values: OptEnumValueDefinitionVec<'a>
});

impl<'a> fmt::Display for EnumTypeExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend enum {}{}{}",
            self.name, self.directives, self.values
        )
    }
}

ast_node_with_location!(
    #[doc = "An input object type extension can be used to add fields to an existing input object"]
    InputObjectTypeExtension {
        #[doc = "The name of the \"target\" input object"]
        name: Name<'a>,
        #[doc = "A list of directives assigned to the input object extension"]
        directives: OptDirectiveVec<'a>,
        #[doc = "A list of fields that the input object extension has"]
        fields: OptInputValueDefinitionVec<'a>,
    }
);

impl<'a> fmt::Display for InputObjectTypeExtension<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend input {}{}{}",
            self.name, self.directives, self.fields
        )
    }
}

impl<'a, 'b, T> From<&'a Box<T>> for AST<'a, 'b> {
    fn from(_s: &'a Box<T>) -> Self {
        panic!("hi")
    }
}
