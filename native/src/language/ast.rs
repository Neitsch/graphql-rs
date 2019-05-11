use super::source::Source;

#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
pub struct Name {
    pub loc: Option<Location>,
    pub value: String,
}

// Document
#[derive(Debug, PartialEq)]
pub struct Document {
    pub loc: Option<Location>,
    pub definitions: Vec<Definition>,
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    ExecutableDefinition(ExecutableDefinition),
    TypeSystemDefinition(TypeSystemDefinition),
    TypeSystemExtension(TypeSystemExtension),
}

#[derive(Debug, PartialEq)]
pub enum ExecutableDefinition {
    OperationDefinition(OperationDefinition),
    FragmentDefinition(FragmentDefinition),
}

#[derive(Debug, PartialEq)]
pub struct OperationDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    pub name: Option<Name>,
    pub variableDefinitions: Option<Vec<VariableDefinition>>,
    pub directives: Option<Vec<Directive>>,
    pub selectionSet: SelectionSet,
}

#[derive(Debug, PartialEq)]
pub enum OperationType {
    QUERY,
    MUTATION,
    SUBSCRIPTION,
}

#[derive(Debug, PartialEq)]
pub struct VariableDefinition {
    pub loc: Option<Location>,
    pub variable: Variable,
    pub _type: Type,
    pub defaultValue: Option<Value>,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub loc: Option<Location>,
    pub name: Name,
}

#[derive(Debug, PartialEq)]
pub struct SelectionSet {
    pub loc: Option<Location>,
    pub selections: Vec<Selection>,
}

#[derive(Debug, PartialEq)]
pub enum Selection {
    Field(Field),
    FragmentSpread(FragmentSpread),
    InlineFragment(InlineFragment),
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub loc: Option<Location>,
    pub alias: Option<Name>,
    pub name: Name,
    pub arguments: Option<Vec<Argument>>,
    pub directives: Option<Vec<Directive>>,
    pub selectionSet: Option<SelectionSet>,
}

#[derive(Debug, PartialEq)]
pub struct Argument {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

// Fragments
#[derive(Debug, PartialEq)]
pub struct FragmentSpread {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct InlineFragment {
    pub loc: Option<Location>,
    pub typeCondition: Option<NamedType>,
    pub directives: Option<Vec<Directive>>,
    pub selectionSet: SelectionSet,
}

#[derive(Debug, PartialEq)]
pub struct FragmentDefinition {
    pub loc: Option<Location>,
    pub name: Name,
    // Note: fragment variable definitions are experimental and may be changed
    // or removed in the future.
    pub variableDefinitions: Option<Vec<VariableDefinition>>,
    pub typeCondition: NamedType,
    pub directives: Option<Vec<Directive>>,
    pub selectionSet: SelectionSet,
}

// Values
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct IntValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct FloatValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct StringValue {
    pub loc: Option<Location>,
    pub value: String,
    pub block: Option<bool>,
}

#[derive(Debug, PartialEq)]
pub struct BooleanValue {
    pub loc: Option<Location>,
    pub value: bool,
}

#[derive(Debug, PartialEq)]
pub struct NullValue {
    pub loc: Option<Location>,
}

#[derive(Debug, PartialEq)]
pub struct EnumValue {
    pub loc: Option<Location>,
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct ListValue {
    pub loc: Option<Location>,
    pub values: Vec<Value>,
}

#[derive(Debug, PartialEq)]
pub struct ObjectValue {
    pub loc: Option<Location>,
    pub fields: Vec<ObjectField>,
}

#[derive(Debug, PartialEq)]
pub struct ObjectField {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

// Directives
#[derive(Debug, PartialEq)]
pub struct Directive {
    pub loc: Option<Location>,
    pub name: Name,
    pub arguments: Option<Vec<Argument>>,
}

// Type Reference
#[derive(Debug, PartialEq)]
pub enum Type {
    NamedType(NamedType),
    ListType(ListType),
    NonNullType(NonNullType),
}

#[derive(Debug, PartialEq)]
pub struct NamedType {
    pub loc: Option<Location>,
    pub name: Name,
}

#[derive(Debug, PartialEq)]
pub struct ListType {
    pub loc: Option<Location>,
    pub _type: Box<Type>,
}

#[derive(Debug, PartialEq)]
pub enum NonNullInnerType {
    NamedType(NamedType),
    ListType(ListType),
}

#[derive(Debug, PartialEq)]
pub struct NonNullType {
    pub loc: Option<Location>,
    pub _type: NonNullInnerType,
}

// Type System Definition
#[derive(Debug, PartialEq)]
pub enum TypeSystemDefinition {
    SchemaDefinition(SchemaDefinition),
    TypeDefinition(TypeDefinition),
    DirectiveDefinition(DirectiveDefinition),
}

#[derive(Debug, PartialEq)]
pub struct SchemaDefinition {
    pub loc: Option<Location>,
    pub directives: Option<Vec<Directive>>,
    pub operationTypes: Vec<OperationTypeDefinition>,
}

#[derive(Debug, PartialEq)]
pub struct OperationTypeDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    pub _type: NamedType,
}

// Type Definition
#[derive(Debug, PartialEq)]
pub enum TypeDefinition {
    ScalarTypeDefinition(ScalarTypeDefinition),
    ObjectTypeDefinition(ObjectTypeDefinition),
    InterfaceTypeDefinition(InterfaceTypeDefinition),
    UnionTypeDefinition(UnionTypeDefinition),
    EnumTypeDefinition(EnumTypeDefinition),
    InputObjectTypeDefinition(InputObjectTypeDefinition),
}

#[derive(Debug, PartialEq)]
pub struct ScalarTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct ObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[derive(Debug, PartialEq)]
pub struct FieldDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub arguments: Option<Vec<InputValueDefinition>>,
    pub _type: Type,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct InputValueDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub _type: Type,
    pub defaultValue: Option<Value>,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct InterfaceTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[derive(Debug, PartialEq)]
pub struct UnionTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub types: Option<Vec<NamedType>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub values: Option<Vec<EnumValueDefinition>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumValueDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct InputObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<InputValueDefinition>>,
}

// Directive Definitions
#[derive(Debug, PartialEq)]
pub struct DirectiveDefinition {
    pub loc: Option<Location>,
    pub description: Option<StringValue>,
    pub name: Name,
    pub arguments: Option<Vec<InputValueDefinition>>,
    pub locations: Vec<Name>,
}

// Type System Extensions
#[derive(Debug, PartialEq)]
pub enum TypeSystemExtension {
    SchemaExtension(SchemaExtension),
    TypeExtension(TypeExtension),
}

#[derive(Debug, PartialEq)]
pub struct SchemaExtension {
    pub loc: Option<Location>,
    pub directives: Option<Vec<Directive>>,
    pub operationTypes: Option<Vec<OperationTypeDefinition>>,
}

// Type Extensions
#[derive(Debug, PartialEq)]
pub enum TypeExtension {
    ScalarTypeExtension(ScalarTypeExtension),
    ObjectTypeExtension(ObjectTypeExtension),
    InterfaceTypeExtension(InterfaceTypeExtension),
    UnionTypeExtension(UnionTypeExtension),
    EnumTypeExtension(EnumTypeExtension),
    InputObjectTypeExtension(InputObjectTypeExtension),
}

#[derive(Debug, PartialEq)]
pub struct ScalarTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
}

#[derive(Debug, PartialEq)]
pub struct ObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[derive(Debug, PartialEq)]
pub struct InterfaceTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<FieldDefinition>>,
}

#[derive(Debug, PartialEq)]
pub struct UnionTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub types: Option<Vec<NamedType>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub values: Option<Vec<EnumValueDefinition>>,
}

#[derive(Debug, PartialEq)]
pub struct InputObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: Option<Vec<Directive>>,
    pub fields: Option<Vec<InputValueDefinition>>,
}
