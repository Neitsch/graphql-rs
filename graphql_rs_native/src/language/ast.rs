use super::source::Source;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    /// The character offset at which this Node begins.
    pub start: u64,

    /// The character offset at which this Node ends.
    pub end: u64,

    /// The Source document the AST represents.
    pub source: Source,
}

impl Location {
    pub fn new(start: usize, end: usize, source: &Source) -> Location {
        Location {
            start: start as u64,
            end: end as u64,
            source: source.clone(),
        }
    }
}

pub trait WithLocation {
    fn with_loc(&mut self, loc: Location);
}

pub trait Visitor {
    fn visit<'a>(&self, node: AST<'a>);
}

pub trait Visitable {
    fn visit<V: Visitor>(&self, visitor: &V);
}

impl Visitable for String {
    fn visit<V: Visitor>(&self, _visitor: &V) {
        
    }
}

impl Visitable for bool {
    fn visit<V: Visitor>(&self, _visitor: &V) {
        
    }
}

impl Visitable for Option<bool> {
    fn visit<V: Visitor>(&self, _visitor: &V) {
        
    }
}

macro_rules! ast_node_with_location {
    ( @ $(#[$sattr:meta])* $name:ident { } -> ($($result:tt)*) ) => (
        #[serde(tag = "kind")]
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        $(#[$sattr])*
        pub struct $name {
            pub loc: Option<Location>,
            $($result)*
        }

        impl WithLocation for $name {
            fn with_loc(&mut self, loc: Location) {
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

        impl Visitable for $name {
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
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub struct $name(pub Option<$type>);

        impl From<Option<$type>> for $name {
            fn from(d: Option<$type>) -> Self {
                $name(d)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                if let $name(Some(name)) = &self {
                    write!(f, "{}", name)?;
                }
                Ok(())
            }
        }

        impl core::ops::Deref for $name {
            type Target = Option<$type>;

            fn deref(self: &'_ Self) -> &'_ Self::Target {
                &self.0
            }
        }

        impl core::ops::DerefMut for $name {
            fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
                &mut self.0
            }
        }

        impl Visitable for $name {
            fn visit<V: Visitor>(&self, visitor: &V) {
                if let $name(Some(v)) = self {
                    v.visit(visitor);
                }
            }
        }
    };

    ( $name:ident ( Vec<$type:ty> )) => {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub struct $name(pub Vec<$type>);

        impl From<Vec<$type>> for $name {
            fn from(d: Vec<$type>) -> Self {
                $name(d)
            }
        }

        impl core::ops::Deref for $name {
            type Target = Vec<$type>;

            fn deref(self: &'_ Self) -> &'_ Self::Target {
                &self.0
            }
        }

        impl core::ops::DerefMut for $name {
            fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
                &mut self.0
            }
        }

        impl Visitable for $name {
            fn visit<V: Visitor>(&self, visitor: &V) {
                for v in &self.0 {
                    v.visit(visitor);
                }
            }
        }
    };

    ( $name:ident ( $type:ty )) => {
        #[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
        pub struct $name(pub $type);

        impl From<$type> for $name {
            fn from(d: $type) -> Self {
                $name(d)
            }
        }

        impl core::ops::Deref for $name {
            type Target = $type;

            fn deref(self: &'_ Self) -> &'_ Self::Target {
                &self.0
            }
        }

        impl core::ops::DerefMut for $name {
            fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
                &mut self.0
            }
        }

        impl Visitable for $name {
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
        pub enum $type {
            $(
                $name(Box<$name>),
            )+
        }

        impl<'a> From<&'a $type> for AST<'a> {
            fn from(v: &'a $type) -> Self {
                match v {
                    $(
                        $type::$name(x) => AST::from(x),
                    )+
                }
            }
        }

        impl fmt::Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $type::$name(x) => write!(f, "{}", x),
                    )+
                }
            }
        }

        impl Visitable for $type {
            fn visit<V: Visitor>(&self, visitor: &V) {
                match self {
                    $(
                        $type::$name(v) => v.visit(visitor),
                    )+
                }
            }
        }

        $(
            impl From<$name> for $type {
                fn from(v: $name) -> Self {
                    $type::$name(Box::new(v))
                }
            }
        )+
    };
}

macro_rules! ast_node_enum {
    ( $type:ident, { $( $name:ident ), * } ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum $type<'a> {
            $(
                $name(&'a $name),
            )+
        }

        impl<'a> fmt::Display for $type<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $type::$name(v) => write!(f, "{}", v),
                    )+
                }
            }
        }

        $(
            impl<'a> From<&'a $name> for $type<'a> {
                fn from(v: &'a $name) -> Self {
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

impl Name {
    pub fn new(name: String, loc: Location) -> Name {
        Name {
            loc: Some(loc),
            value: name,
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

wrapping_ast_node!(OptName(Option<Name>));

wrapping_ast_node!(NameVec(Vec<Name>));

// Document
ast_node_with_location!(Document {
    definitions: DefinitionVec,
});

impl fmt::Display for Document {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.definitions,)
    }
}

ast_enum!(Definition, {
    ExecutableDefinition,
    TypeSystemDefinition,
    TypeSystemExtension
});

wrapping_ast_node!(DefinitionVec(Vec<Definition>));

impl fmt::Display for DefinitionVec {
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
    name: OptName,
    #[serde(rename = "variableDefinitions")]
    variable_definitions: OptVariableDefinitionVec,
    directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    selection_set: SelectionSet
});

impl fmt::Display for OperationDefinition {
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
    fn visit<V: Visitor>(&self, _v: &V) {

    }
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
    variable: Variable,
    #[serde(rename = "type")]
    _type: Type,
    #[serde(rename = "defaultValue")]
    default_value: OptValue,
    directives: OptDirectiveVec
});

impl fmt::Display for VariableDefinition {
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

wrapping_ast_node!(VariableDefinitionVec(Vec<VariableDefinition>));

impl fmt::Display for VariableDefinitionVec {
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

wrapping_ast_node!(OptVariableDefinitionVec(Option<VariableDefinitionVec>));

ast_node_with_location!(Variable { name: Name });

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name)
    }
}

ast_node_with_location!(SelectionSet {
     selections: SelectionVec
});

wrapping_ast_node!(OptSelectionSet(Option<SelectionSet>));

impl fmt::Display for SelectionSet {
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

wrapping_ast_node!(SelectionVec(Vec<Selection>));

ast_node_with_location!(Field {
    alias: OptName,
    name: Name,
    arguments: OptArgumentVec,
    directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    selection_set: OptSelectionSet
});

impl fmt::Display for Field {
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
    name: Name,
    value: Value
});

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

wrapping_ast_node!(ArgumentVec(Vec<Argument>));

impl fmt::Display for ArgumentVec {
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

wrapping_ast_node!(OptArgumentVec(Option<ArgumentVec>));

impl From<Option<Vec<Argument>>> for OptArgumentVec {
    fn from(d: Option<Vec<Argument>>) -> Self {
        OptArgumentVec(d.map(std::convert::Into::into))
    }
}

// Fragments
ast_node_with_location!(FragmentSpread {
    name: Name,
    directives: OptDirectiveVec
});

impl fmt::Display for FragmentSpread {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...{}{}", self.name, self.directives)
    }
}

ast_node_with_location!(InlineFragment {
    #[serde(rename = "typeCondition")]
    type_condition: OptNamedType,
    directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    selection_set: SelectionSet
});

impl fmt::Display for InlineFragment {
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
    name: Name,
    #[serde(rename = "variableDefinitions")]
    variable_definitions: OptVariableDefinitionVec,
    #[serde(rename = "typeCondition")]
    type_condition: NamedType,
    directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    selection_set: SelectionSet,
});

impl fmt::Display for FragmentDefinition {
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

wrapping_ast_node!(OptValue(Option<Value>));

wrapping_ast_node!(ValueVec(Vec<Value>));

impl fmt::Display for ValueVec {
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

impl fmt::Display for IntValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

ast_node_with_location!(FloatValue { value: String });

impl fmt::Display for FloatValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

ast_node_with_location!(StringValue {
    value: String,
    block: Option<bool>
});

impl fmt::Display for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let delimiter = match &self.block {
            Some(true) => "\"\"\"",
            _ => "\"",
        };
        write!(f, "{}{}{}", delimiter, self.value, delimiter)
    }
}

wrapping_ast_node!(Description(Option<StringValue>));

ast_node_with_location!(BooleanValue { value: bool });

impl fmt::Display for BooleanValue {
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

impl fmt::Display for NullValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NULL")
    }
}

ast_node_with_location!(EnumValue { value: String });

impl fmt::Display for EnumValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

ast_node_with_location!(ListValue { values: ValueVec });

impl fmt::Display for ListValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.values)
    }
}

ast_node_with_location!(ObjectValue {
    fields: ObjectFieldVec
});

impl fmt::Display for ObjectValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fields)
    }
}

ast_node_with_location!(ObjectField {
    name: Name,
    value: Value
});

impl fmt::Display for ObjectField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

wrapping_ast_node!(ObjectFieldVec(Vec<ObjectField>));

impl fmt::Display for ObjectFieldVec {
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
    name: Name,
    arguments: OptArgumentVec,
});

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)?;
        if let OptArgumentVec(Some(args)) = &self.arguments {
            write!(f, "{}", args)?;
        }
        Ok(())
    }
}

wrapping_ast_node!(DirectiveVec(Vec<Directive>));

impl fmt::Display for DirectiveVec {
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

wrapping_ast_node!(OptDirectiveVec(Option<DirectiveVec>));

impl From<Option<Vec<Directive>>> for OptDirectiveVec {
    fn from(d: Option<Vec<Directive>>) -> Self {
        OptDirectiveVec(d.map(std::convert::Into::into))
    }
}

// Type Reference
ast_enum!(Type, {
    NamedType,
    ListType,
    NonNullType
});

ast_node_with_location!(NamedType { name: Name });

wrapping_ast_node!(NamedTypeVec(Vec<NamedType>));

impl fmt::Display for NamedTypeVec {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        panic!("Cannot format this");
    }
}

wrapping_ast_node!(OptNamedTypeVec(Option<NamedTypeVec>));

impl fmt::Display for NamedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl From<Name> for NamedType {
    fn from(n: Name) -> Self {
        NamedType {
            loc: n.loc.clone(),
            name: n,
        }
    }
}

wrapping_ast_node!(OptNamedType(Option<NamedType>));

ast_node_with_location!(ListType {
    #[serde(rename = "type")]
    _type: Box<Type>,
});

impl fmt::Display for ListType {
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
    _type: NonNullInnerType
});

impl fmt::Display for NonNullType {
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
    directives: OptDirectiveVec,
    #[serde(rename = "operationTypes")]
    operation_types: OperationTypeDefinitionVec
});

impl fmt::Display for SchemaDefinition {
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
    _type: NamedType
});

impl fmt::Display for OperationTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.operation, self._type)
    }
}

wrapping_ast_node!(OperationTypeDefinitionVec(Vec<OperationTypeDefinition>));

impl fmt::Display for OperationTypeDefinitionVec {
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

wrapping_ast_node!(OptOperationTypeDefinitionVec(Option<OperationTypeDefinitionVec>));

impl From<Option<Vec<OperationTypeDefinition>>> for OptOperationTypeDefinitionVec {
    fn from(d: Option<Vec<OperationTypeDefinition>>) -> Self {
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
    description: Description,
    name: Name,
    directives: OptDirectiveVec
});

impl fmt::Display for ScalarTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}scalar {}{}",
            self.description, self.name, self.directives
        )
    }
}

ast_node_with_location!(ObjectTypeDefinition {
    description: Description,
    name: Name,
    interfaces: InterfaceNamedTypeVec,
    directives: OptDirectiveVec,
    fields: OptFieldDefinitionVec
});

wrapping_ast_node!(InterfaceNamedTypeVec(OptNamedTypeVec));

impl From<Option<Vec<NamedType>>> for InterfaceNamedTypeVec {
    fn from(d: Option<Vec<NamedType>>) -> Self {
        InterfaceNamedTypeVec(OptNamedTypeVec::from(d))
    }
}

impl From<Option<Vec<NamedType>>> for OptNamedTypeVec {
    fn from(d: Option<Vec<NamedType>>) -> Self {
        OptNamedTypeVec(d.map(|v| v.into()))
    }
}

impl fmt::Display for ObjectTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}type {}{}{}{}",
            self.description, self.name, self.interfaces, self.directives, self.fields
        )
    }
}

impl fmt::Display for InterfaceNamedTypeVec {
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
            write!(
                f, ""
            )
        }
    }
}

ast_node_with_location!(FieldDefinition {
    description: Description,
    name: Name,
    arguments: OptInputValueDefinitionVec,
    #[serde(rename = "type")]
    _type: Type,
    directives: OptDirectiveVec
});

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}: {}{}",
            self.description, self.name, self.arguments, self._type, self.directives
        )
    }
}

wrapping_ast_node!(FieldDefinitionVec(Vec<FieldDefinition>));

impl fmt::Display for FieldDefinitionVec {
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

wrapping_ast_node!(OptFieldDefinitionVec(Option<FieldDefinitionVec>));

impl From<Option<Vec<FieldDefinition>>> for OptFieldDefinitionVec {
    fn from(d: Option<Vec<FieldDefinition>>) -> Self {
        OptFieldDefinitionVec(d.map(std::convert::Into::into))
    }
}

impl From<Vec<FieldDefinition>> for OptFieldDefinitionVec {
    fn from(v: Vec<FieldDefinition>) -> Self {
        Some(v).into()
    }
}

ast_node_with_location!(InputValueDefinition {
    description: Description,
    name: Name,
    #[serde(rename = "type")]
    _type: Type,
    #[serde(rename = "defaultValue")]
    default_value: OptValue,
    directives: OptDirectiveVec
});

impl fmt::Display for InputValueDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}: {}{}{}",
            self.description, self.name, self._type, self.default_value, self.directives
        )
    }
}

wrapping_ast_node!(InputValueDefinitionVec(Vec<InputValueDefinition>));

impl fmt::Display for InputValueDefinitionVec {
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

wrapping_ast_node!(OptInputValueDefinitionVec(Option<InputValueDefinitionVec>));

impl From<Option<Vec<InputValueDefinition>>> for OptInputValueDefinitionVec {
    fn from(d: Option<Vec<InputValueDefinition>>) -> Self {
        OptInputValueDefinitionVec(d.map(std::convert::Into::into))
    }
}

ast_node_with_location!(InterfaceTypeDefinition {
    description: Description,
    name: Name,
    directives: OptDirectiveVec,
    fields: OptFieldDefinitionVec
});

impl fmt::Display for InterfaceTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}interface {}{}{}",
            self.description, self.name, self.directives, self.fields
        )
    }
}

ast_node_with_location!(UnionTypeDefinition {
    description: Description,
    name: Name,
    directives: OptDirectiveVec,
    types: UnionNamedTypeVec
});

wrapping_ast_node!(UnionNamedTypeVec(OptNamedTypeVec));

impl From<Option<Vec<NamedType>>> for UnionNamedTypeVec {
    fn from(d: Option<Vec<NamedType>>) -> Self {
        UnionNamedTypeVec(OptNamedTypeVec::from(d))
    }
}

impl fmt::Display for UnionTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}union {}{} = {}",
            self.description, self.name, self.directives, self.types
        )
    }
}

impl fmt::Display for UnionNamedTypeVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>)-> fmt::Result {
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
            write!(
                f, ""
            )
        }
    }
}

ast_node_with_location!(EnumTypeDefinition {
    description: Description,
    name: Name,
    directives: OptDirectiveVec,
    values: OptEnumValueDefinitionVec
});

impl fmt::Display for EnumTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}enum {}{}{}",
            self.description, self.name, self.directives, self.values
        )
    }
}

ast_node_with_location!(EnumValueDefinition {
    description: Description,
    name: Name,
    directives: OptDirectiveVec
});

impl fmt::Display for EnumValueDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.description, self.name, self.directives)
    }
}

wrapping_ast_node!(EnumValueDefinitionVec(Vec<EnumValueDefinition>));

impl fmt::Display for EnumValueDefinitionVec {
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

wrapping_ast_node!(OptEnumValueDefinitionVec(Option<EnumValueDefinitionVec>));

impl From<Option<Vec<EnumValueDefinition>>> for OptEnumValueDefinitionVec {
    fn from(d: Option<Vec<EnumValueDefinition>>) -> Self {
        OptEnumValueDefinitionVec(d.map(std::convert::Into::into))
    }
}

ast_node_with_location!(InputObjectTypeDefinition {
    description: Description,
    name: Name,
    directives: OptDirectiveVec,
    fields: OptInputValueDefinitionVec
});

impl fmt::Display for InputObjectTypeDefinition {
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
    description: Description,
    name: Name,
    arguments: OptInputValueDefinitionVec,
    locations: NameVec
});

impl fmt::Display for DirectiveDefinition {
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
    directives: OptDirectiveVec,
    #[serde(rename = "operationTypes")]
    operation_types: OptOperationTypeDefinitionVec
});

impl fmt::Display for SchemaExtension {
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
    name: Name,
    directives: OptDirectiveVec
});

impl fmt::Display for ScalarTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extend scalar{}{}", self.name, self.directives)
    }
}

ast_node_with_location!(ObjectTypeExtension {
    name: Name,
    interfaces: InterfaceNamedTypeVec,
    directives: OptDirectiveVec,
    fields: OptFieldDefinitionVec
});

impl fmt::Display for ObjectTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend type {}{}{}{}",
            self.name, self.interfaces, self.directives, self.fields
        )
    }
}

ast_node_with_location!(InterfaceTypeExtension {
    name: Name,
    directives: OptDirectiveVec,
    fields: OptFieldDefinitionVec
});

impl fmt::Display for InterfaceTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend interface {}{}{}",
            self.name, self.directives, self.fields
        )
    }
}

ast_node_with_location!(UnionTypeExtension {
    name: Name,
    directives: OptDirectiveVec,
    types: UnionNamedTypeVec
});

impl fmt::Display for UnionTypeExtension {
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
    name: Name,
    #[doc = "A list of directives assigned to the enum extension"]
    directives: OptDirectiveVec,
    #[doc = "A list of possible values to extend the enum by."]
    values: OptEnumValueDefinitionVec
});

impl fmt::Display for EnumTypeExtension {
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
        name: Name,
        #[doc = "A list of directives assigned to the input object extension"]
        directives: OptDirectiveVec,
        #[doc = "A list of fields that the input object extension has"]
        fields: OptInputValueDefinitionVec,
    }
);

impl fmt::Display for InputObjectTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend input {}{}{}",
            self.name, self.directives, self.fields
        )
    }
}

impl<'a, T> From<&'a Box<T>> for AST<'a> {
    fn from(_s: &'a Box<T>) -> Self {
        panic!("hi")
    }
}
