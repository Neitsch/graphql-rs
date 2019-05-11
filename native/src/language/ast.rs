use super::source::Source;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum AST {
    Name(Name),
    Document(Document),
    OperationDefinition(OperationDefinition),
    VariableDefinition(VariableDefinition),
    Variable(Variable),
    SelectionSet(SelectionSet),
    Field(Field),
    Argument(Argument),
    FragmentSpread(FragmentSpread),
    InlineFragment(InlineFragment),
    FragmentDefinition(FragmentDefinition),
    IntValue(IntValue),
    FloatValue(FloatValue),
    StringValue(StringValue),
    BooleanValue(BooleanValue),
    NullValue(NullValue),
    EnumValue(EnumValue),
    ListValue(ListValue),
    ObjectValue(ObjectValue),
    ObjectField(ObjectField),
    Directive(Directive),
    NamedType(NamedType),
    ListType(ListType),
    NonNullType(NonNullType),
    SchemaDefinition(SchemaDefinition),
    OperationTypeDefinition(OperationTypeDefinition),
    ScalarTypeDefinition(ScalarTypeDefinition),
    ObjectTypeDefinition(ObjectTypeDefinition),
    FieldDefinition(FieldDefinition),
    InputValueDefinition(InputValueDefinition),
    InterfaceTypeDefinition(InterfaceTypeDefinition),
    UnionTypeDefinition(UnionTypeDefinition),
    EnumTypeDefinition(EnumTypeDefinition),
    EnumValueDefinition(EnumValueDefinition),
    InputObjectTypeDefinition(InputObjectTypeDefinition),
    DirectiveDefinition(DirectiveDefinition),
    SchemaExtension(SchemaExtension),
    ScalarTypeExtension(ScalarTypeExtension),
    ObjectTypeExtension(ObjectTypeExtension),
    InterfaceTypeExtension(InterfaceTypeExtension),
    UnionTypeExtension(UnionTypeExtension),
    EnumTypeExtension(EnumTypeExtension),
    InputObjectTypeExtension(InputObjectTypeExtension),
}

// Name
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Name {
    pub loc: Option<Location>,
    pub value: String,
}

// Document
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Document {
    pub loc: Option<Location>,
    pub definitions: Vec<Definition>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Definition {
    ExecutableDefinition(ExecutableDefinition),
    TypeSystemDefinition(TypeSystemDefinition),
    TypeSystemExtension(TypeSystemExtension),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ExecutableDefinition {
    OperationDefinition(OperationDefinition),
    FragmentDefinition(FragmentDefinition),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct OperationDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    pub name: Option<Name>,
    #[serde(rename="variableDefinitions")]
    pub variable_definitions: Option<Vec<VariableDefinition>>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename="selectionSet")]
    pub selection_set: SelectionSet,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum OperationType {
    #[serde(rename="query")]
    QUERY,
    #[serde(rename="mutation")]
    MUTATION,
    #[serde(rename="subscription")]
    SUBSCRIPTION,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct VariableDefinition {
    pub loc: Option<Location>,
    pub variable: Variable,
    #[serde(rename="type")]
    pub _type: Type,
    #[serde(rename="defaultValue")]
    pub default_value: Option<Value>,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Variable {
    pub loc: Option<Location>,
    pub name: Name,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct SelectionSet {
    pub loc: Option<Location>,
    pub selections: Vec<Selection>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Selection {
    Field(Field),
    FragmentSpread(FragmentSpread),
    InlineFragment(InlineFragment),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub loc: Option<Location>,
    pub alias: Option<Name>,
    pub name: Name,
    pub arguments: Option<Vec<Argument>>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename="selectionSet")]
    pub selection_set: Option<SelectionSet>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

// Fragments
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FragmentSpread {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InlineFragment {
    pub loc: Option<Location>,
    #[serde(rename="typeCondition")]
    pub type_condition: Option<NamedType>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename="selectionSet")]
    pub selection_set: SelectionSet,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FragmentDefinition {
    pub loc: Option<Location>,
    pub name: Name,
    // Note: fragment variable definitions are experimental and may be changed
    // or removed in the future.
    #[serde(rename="variableDefinitions")]
    pub variable_definitions: Option<Vec<VariableDefinition>>,
    #[serde(rename="typeCondition")]
    pub type_condition: NamedType,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename="selectionSet")]
    pub selection_set: SelectionSet,
}

// Values
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Variable(Variable),
    IntValue(IntValue),
    FloatValue(FloatValue),
    StringValue(StringValue),
    BooleanValue(BooleanValue),
    NullValue(NullValue),
    EnumValue(EnumValue),
    ListValue(ListValue),
    ObjectValue(ObjectValue),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct IntValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FloatValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct StringValue {
    pub loc: Option<Location>,
    pub value: String,
    pub block: Option<bool>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct BooleanValue {
    pub loc: Option<Location>,
    pub value: bool,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct NullValue {
    pub loc: Option<Location>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EnumValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ListValue {
    pub loc: Option<Location>,
    pub values: Vec<Value>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ObjectValue {
    pub loc: Option<Location>,
    pub fields: Vec<ObjectField>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ObjectField {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

// Directives
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Directive {
    pub loc: Option<Location>,
    pub name: Name,
    pub arguments: Option<Vec<Argument>>,
}

// Type Reference
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Type {
    NamedType(NamedType),
    ListType(ListType),
    NonNullType(NonNullType),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct NamedType {
    pub loc: Option<Location>,
    pub name: Name,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ListType {
    pub loc: Option<Location>,
    #[serde(rename="type")]
    pub _type: Box<Type>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum NonNullInnerType {
    NamedType(NamedType),
    ListType(ListType),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct NonNullType {
    pub loc: Option<Location>,
    #[serde(rename="type")]
    pub _type: NonNullInnerType,
}

// Type System Definition
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TypeSystemDefinition {
    SchemaDefinition(SchemaDefinition),
    TypeDefinition(TypeDefinition),
    DirectiveDefinition(DirectiveDefinition),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct SchemaDefinition {
    pub loc: Option<Location>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename="operationTypes")]
    pub operation_types: Vec<OperationTypeDefinition>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct OperationTypeDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    #[serde(rename="type")]
    pub _type: NamedType,
}

// Type Definition
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TypeDefinition {
    ScalarTypeDefinition(ScalarTypeDefinition),
    ObjectTypeDefinition(ObjectTypeDefinition),
    InterfaceTypeDefinition(InterfaceTypeDefinition),
    UnionTypeDefinition(UnionTypeDefinition),
    EnumTypeDefinition(EnumTypeDefinition),
    InputObjectTypeDefinition(InputObjectTypeDefinition),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ScalarTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct FieldDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub arguments: Option<Vec<InputValueDefinition>>,
    #[serde(rename="type")]
    pub _type: Type,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InputValueDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    #[serde(rename="type")]
    pub _type: Type,
    #[serde(rename="defaultValue")]
    pub default_value: Option<Value>,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InterfaceTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct UnionTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub types: Option<Vec<NamedType>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EnumTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub values: Option<Vec<EnumValueDefinition>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EnumValueDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InputObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<InputValueDefinition>>,
}

// Directive Definitions
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct DirectiveDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub arguments: Option<Vec<InputValueDefinition>>,
    pub locations: Vec<Name>,
}

// Type System Extensions
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TypeSystemExtension {
    SchemaExtension(SchemaExtension),
    TypeExtension(TypeExtension),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct SchemaExtension {
    pub loc: Option<Location>,
    pub directives: Option<Vec<Directive>>,
    #[serde(rename="operationTypes")]
    pub operation_types: Option<Vec<OperationTypeDefinition>>,
}

// Type Extensions
#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TypeExtension {
    ScalarTypeExtension(ScalarTypeExtension),
    ObjectTypeExtension(ObjectTypeExtension),
    InterfaceTypeExtension(InterfaceTypeExtension),
    UnionTypeExtension(UnionTypeExtension),
    EnumTypeExtension(EnumTypeExtension),
    InputObjectTypeExtension(InputObjectTypeExtension),
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ScalarTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InterfaceTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct UnionTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub types: Option<Vec<NamedType>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct EnumTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub values: Option<Vec<EnumValueDefinition>>,
}

#[serde(tag="kind")]
#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct InputObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<InputValueDefinition>>,
}
