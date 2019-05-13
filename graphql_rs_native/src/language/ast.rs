use super::source::Source;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Location {
    /// The character offset at which this Node begins.
    pub start: u64,

    /// The character offset at which this Node ends.
    pub end: u64,

    /// The Source document the AST represents.
    pub source: Source,
}

/**
 * The list of all possible AST node types.
 */
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum AST {
    Name(Box<Name>),
    Document(Box<Document>),
    OperationDefinition(Box<OperationDefinition>),
    VariableDefinition(Box<VariableDefinition>),
    Variable(Box<Variable>),
    SelectionSet(Box<SelectionSet>),
    Field(Box<Field>),
    Argument(Box<Argument>),
    FragmentSpread(Box<FragmentSpread>),
    InlineFragment(Box<InlineFragment>),
    FragmentDefinition(Box<FragmentDefinition>),
    IntValue(Box<IntValue>),
    FloatValue(Box<FloatValue>),
    StringValue(Box<StringValue>),
    BooleanValue(Box<BooleanValue>),
    NullValue(Box<NullValue>),
    EnumValue(Box<EnumValue>),
    ListValue(Box<ListValue>),
    ObjectValue(Box<ObjectValue>),
    ObjectField(Box<ObjectField>),
    Directive(Box<Directive>),
    NamedType(Box<NamedType>),
    ListType(Box<ListType>),
    NonNullType(Box<NonNullType>),
    SchemaDefinition(Box<SchemaDefinition>),
    OperationTypeDefinition(Box<OperationTypeDefinition>),
    ScalarTypeDefinition(Box<ScalarTypeDefinition>),
    ObjectTypeDefinition(Box<ObjectTypeDefinition>),
    FieldDefinition(Box<FieldDefinition>),
    InputValueDefinition(Box<InputValueDefinition>),
    InterfaceTypeDefinition(Box<InterfaceTypeDefinition>),
    UnionTypeDefinition(Box<UnionTypeDefinition>),
    EnumTypeDefinition(Box<EnumTypeDefinition>),
    EnumValueDefinition(Box<EnumValueDefinition>),
    InputObjectTypeDefinition(Box<InputObjectTypeDefinition>),
    DirectiveDefinition(Box<DirectiveDefinition>),
    SchemaExtension(Box<SchemaExtension>),
    ScalarTypeExtension(Box<ScalarTypeExtension>),
    ObjectTypeExtension(Box<ObjectTypeExtension>),
    InterfaceTypeExtension(Box<InterfaceTypeExtension>),
    UnionTypeExtension(Box<UnionTypeExtension>),
    EnumTypeExtension(Box<EnumTypeExtension>),
    InputObjectTypeExtension(Box<InputObjectTypeExtension>),
}

// Name
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Name {
    pub loc: Option<Location>,
    pub value: String,
}

// Document
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Document {
    pub loc: Option<Location>,
    pub definitions: Vec<Definition>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Definition {
    ExecutableDefinition(Box<ExecutableDefinition>),
    TypeSystemDefinition(Box<TypeSystemDefinition>),
    TypeSystemExtension(Box<TypeSystemExtension>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExecutableDefinition {
    OperationDefinition(Box<OperationDefinition>),
    FragmentDefinition(Box<FragmentDefinition>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperationDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    pub name: Option<Name>,
    #[serde(rename = "variableDefinitions")]
    pub variable_definitions: Option<Vec<VariableDefinition>>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename = "selectionSet")]
    pub selection_set: SelectionSet,
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

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDefinition {
    pub loc: Option<Location>,
    pub variable: Variable,
    #[serde(rename = "type")]
    pub _type: Type,
    #[serde(rename = "defaultValue")]
    pub default_value: Option<Value>,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variable {
    pub loc: Option<Location>,
    pub name: Name,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectionSet {
    pub loc: Option<Location>,
    pub selections: Vec<Selection>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Selection {
    Field(Box<Field>),
    FragmentSpread(Box<FragmentSpread>),
    InlineFragment(Box<InlineFragment>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub loc: Option<Location>,
    pub alias: Option<Name>,
    pub name: Name,
    pub arguments: Option<Vec<Argument>>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename = "selectionSet")]
    pub selection_set: Option<SelectionSet>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

// Fragments
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FragmentSpread {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InlineFragment {
    pub loc: Option<Location>,
    #[serde(rename = "typeCondition")]
    pub type_condition: Option<NamedType>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename = "selectionSet")]
    pub selection_set: SelectionSet,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FragmentDefinition {
    pub loc: Option<Location>,
    pub name: Name,
    // Note: fragment variable definitions are experimental and may be changed
    // or removed in the future.
    #[serde(rename = "variableDefinitions")]
    pub variable_definitions: Option<Vec<VariableDefinition>>,
    #[serde(rename = "typeCondition")]
    pub type_condition: NamedType,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename = "selectionSet")]
    pub selection_set: SelectionSet,
}

// Values
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Variable(Box<Variable>),
    IntValue(Box<IntValue>),
    FloatValue(Box<FloatValue>),
    StringValue(Box<StringValue>),
    BooleanValue(Box<BooleanValue>),
    NullValue(Box<NullValue>),
    EnumValue(Box<EnumValue>),
    ListValue(Box<ListValue>),
    ObjectValue(Box<ObjectValue>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IntValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FloatValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StringValue {
    pub loc: Option<Location>,
    pub value: String,
    pub block: Option<bool>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BooleanValue {
    pub loc: Option<Location>,
    pub value: bool,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NullValue {
    pub loc: Option<Location>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListValue {
    pub loc: Option<Location>,
    pub values: Vec<Value>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectValue {
    pub loc: Option<Location>,
    pub fields: Vec<ObjectField>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectField {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

// Directives
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Directive {
    pub loc: Option<Location>,
    pub name: Name,
    pub arguments: Option<Vec<Argument>>,
}

// Type Reference
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    NamedType(Box<NamedType>),
    ListType(Box<ListType>),
    NonNullType(Box<NonNullType>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NamedType {
    pub loc: Option<Location>,
    pub name: Name,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListType {
    pub loc: Option<Location>,
    #[serde(rename = "type")]
    pub _type: Box<Type>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NonNullInnerType {
    NamedType(Box<NamedType>),
    ListType(Box<ListType>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NonNullType {
    pub loc: Option<Location>,
    #[serde(rename = "type")]
    pub _type: NonNullInnerType,
}

// Type System Definition
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSystemDefinition {
    SchemaDefinition(Box<SchemaDefinition>),
    TypeDefinition(Box<TypeDefinition>),
    DirectiveDefinition(Box<DirectiveDefinition>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SchemaDefinition {
    pub loc: Option<Location>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename = "operationTypes")]
    pub operation_types: Vec<OperationTypeDefinition>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperationTypeDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    #[serde(rename = "type")]
    pub _type: NamedType,
}

// Type Definition
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDefinition {
    ScalarTypeDefinition(Box<ScalarTypeDefinition>),
    ObjectTypeDefinition(Box<ObjectTypeDefinition>),
    InterfaceTypeDefinition(Box<InterfaceTypeDefinition>),
    UnionTypeDefinition(Box<UnionTypeDefinition>),
    EnumTypeDefinition(Box<EnumTypeDefinition>),
    InputObjectTypeDefinition(Box<InputObjectTypeDefinition>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ScalarTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub arguments: Option<Vec<InputValueDefinition>>,
    #[serde(rename = "type")]
    pub _type: Type,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputValueDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    #[serde(rename = "type")]
    pub _type: Type,
    #[serde(rename = "defaultValue")]
    pub default_value: Option<Value>,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnionTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub types: Option<Vec<NamedType>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub values: Option<Vec<EnumValueDefinition>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumValueDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<InputValueDefinition>>,
}

// Directive Definitions
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DirectiveDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub arguments: Option<Vec<InputValueDefinition>>,
    pub locations: Vec<Name>,
}

// Type System Extensions
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSystemExtension {
    SchemaExtension(Box<SchemaExtension>),
    TypeExtension(Box<TypeExtension>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SchemaExtension {
    pub loc: Option<Location>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename = "operationTypes")]
    pub operation_types: Option<Vec<OperationTypeDefinition>>,
}

// Type Extensions
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeExtension {
    ScalarTypeExtension(Box<ScalarTypeExtension>),
    ObjectTypeExtension(Box<ObjectTypeExtension>),
    InterfaceTypeExtension(Box<InterfaceTypeExtension>),
    UnionTypeExtension(Box<UnionTypeExtension>),
    EnumTypeExtension(Box<EnumTypeExtension>),
    InputObjectTypeExtension(Box<InputObjectTypeExtension>),
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ScalarTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnionTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub types: Option<Vec<NamedType>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub values: Option<Vec<EnumValueDefinition>>,
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<InputValueDefinition>>,
}
