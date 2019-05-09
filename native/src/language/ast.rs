use super::source::{Source};

struct Location {
    /// The character offset at which this Node begins.
    start: u64,

    /// The character offset at which this Node ends.
    end: u64,

    /// The Source document the AST represents.
    source: Source,
}

/**
 * The list of all possible AST node types.
 */
enum AST {
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

struct Name {
    loc: Option<Location>,
    value: String,
}

// Document

struct Document {
    loc: Option<Location>,
    definitions: Vec<Definition>,
}

enum Definition {
    ExecutableDefinition(ExecutableDefinition),
    TypeSystemDefinition(TypeSystemDefinition),
    TypeSystemExtension(TypeSystemExtension),
}

enum ExecutableDefinition {
    OperationDefinition(OperationDefinition),
    FragmentDefinition(FragmentDefinition),
}

struct OperationDefinition {
    loc: Option<Location>,
    operation: OperationType,
    name: Option<Name>,
    variableDefinitions: Option<Vec<VariableDefinition>>,
    directives: Option<Vec<Directive>>,
    selectionSet: SelectionSet,
}

enum OperationType {
    QUERY,
    MUTATION,
    SUBSCRIPTION,
}

struct VariableDefinition {
    loc: Option<Location>,
    variable: Variable,
    _type: Type,
    defaultValue: Option<Value>,
    directives: Option<Vec<Directive>>,
}

struct Variable {
    loc: Option<Location>,
    name: Name,
}

struct SelectionSet {
    loc: Option<Location>,
    selections: Vec<Selection>,
}

enum Selection {
    Field(Field),
    FragmentSpread(FragmentSpread),
    InlineFragment(InlineFragment),
}

struct Field {
    loc: Option<Location>,
    alias: Option<Name>,
    name: Name,
    arguments: Option<Vec<Argument>>,
    directives: Option<Vec<Directive>>,
    selectionSet: Option<SelectionSet>,
}

struct Argument {
    loc: Option<Location>,
    name: Name,
    value: Value,
}

// Fragments

struct FragmentSpread {
    loc: Option<Location>,
    name: Name,
    directives: Option<Vec<Directive>>,
}

struct InlineFragment {
    loc: Option<Location>,
    typeCondition: Option<NamedType>,
    directives: Option<Vec<Directive>>,
    selectionSet: SelectionSet,
}

struct FragmentDefinition {
    loc: Option<Location>,
    name: Name,
    // Note: fragment variable definitions are experimental and may be changed
    // or removed in the future.
    variableDefinitions: Option<Vec<VariableDefinition>>,
    typeCondition: NamedType,
    directives: Option<Vec<Directive>>,
    selectionSet: SelectionSet,
}

// Values

enum Value {
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

struct IntValue {
    loc: Option<Location>,
    value: String,
}

struct FloatValue {
    loc: Option<Location>,
    value: String,
}

struct StringValue {
    loc: Option<Location>,
    value: String,
    block: Option<bool>,
}

struct BooleanValue {
    loc: Option<Location>,
    value: bool,
}

struct NullValue {
    loc: Option<Location>,
}

struct EnumValue {
    loc: Option<Location>,
    value: String,
}

struct ListValue {
    loc: Option<Location>,
    values: Vec<Value>,
}

struct ObjectValue {
    loc: Option<Location>,
    fields: Vec<ObjectField>,
}

struct ObjectField {
    loc: Option<Location>,
    name: Name,
    value: Value,
}

// Directives

struct Directive {
    loc: Option<Location>,
    name: Name,
    arguments: Option<Vec<Argument>>,
}

// Type Reference

enum Type {
    NamedType(NamedType),
    ListType(ListType),
    NonNullType(NonNullType),
}

struct NamedType {
    loc: Option<Location>,
    name: Name,
}

struct ListType {
    loc: Option<Location>,
    _type: Box<Type>,
}

enum NonNullInnerType {
    NamedType(NamedType),
    ListType(ListType),
}

struct NonNullType {
    loc: Option<Location>,
    _type: NonNullInnerType,
}

// Type System Definition

enum TypeSystemDefinition {
    SchemaDefinition(SchemaDefinition),
    TypeDefinition(TypeDefinition),
    DirectiveDefinition(DirectiveDefinition),
}

struct SchemaDefinition {
    loc: Option<Location>,
    directives: Option<Vec<Directive>>,
    operationTypes: Vec<OperationTypeDefinition>,
}

struct OperationTypeDefinition {
    loc: Option<Location>,
    operation: OperationType,
    _type: NamedType,
}

// Type Definition

enum TypeDefinition {
    ScalarTypeDefinition(ScalarTypeDefinition),
    ObjectTypeDefinition(ObjectTypeDefinition),
    InterfaceTypeDefinition(InterfaceTypeDefinition),
    UnionTypeDefinition(UnionTypeDefinition),
    EnumTypeDefinition(EnumTypeDefinition),
    InputObjectTypeDefinition(InputObjectTypeDefinition),
}

struct ScalarTypeDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    directives: Option<Vec<Directive>>,
}

struct ObjectTypeDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    interfaces: Option<Vec<NamedType>>,
    directives: Option<Vec<Directive>>,
    fields: Option<Vec<FieldDefinition>>,
}

struct FieldDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    arguments: Option<Vec<InputValueDefinition>>,
    _type: Type,
    directives: Option<Vec<Directive>>,
}

struct InputValueDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    _type: Type,
    defaultValue: Option<Value>,
    directives: Option<Vec<Directive>>,
}

struct InterfaceTypeDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    directives: Option<Vec<Directive>>,
    fields: Option<Vec<FieldDefinition>>,
}

struct UnionTypeDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    directives: Option<Vec<Directive>>,
    types: Option<Vec<NamedType>>,
}

struct EnumTypeDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    directives: Option<Vec<Directive>>,
    values: Option<Vec<EnumValueDefinition>>,
}

struct EnumValueDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    directives: Option<Vec<Directive>>,
}

struct InputObjectTypeDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    directives: Option<Vec<Directive>>,
    fields: Option<Vec<InputValueDefinition>>,
}

// Directive Definitions

struct DirectiveDefinition {
    loc: Option<Location>,
    description: Option<StringValue>,
    name: Name,
    arguments: Option<Vec<InputValueDefinition>>,
    locations: Vec<Name>,
}

// Type System Extensions

enum TypeSystemExtension {
    SchemaExtension(SchemaExtension),
    TypeExtension(TypeExtension),
}

struct SchemaExtension {
    loc: Option<Location>,
    directives: Option<Vec<Directive>>,
    operationTypes: Option<Vec<OperationTypeDefinition>>,
}

// Type Extensions

enum TypeExtension {
    ScalarTypeExtension(ScalarTypeExtension),
    ObjectTypeExtension(ObjectTypeExtension),
    InterfaceTypeExtension(InterfaceTypeExtension),
    UnionTypeExtension(UnionTypeExtension),
    EnumTypeExtension(EnumTypeExtension),
    InputObjectTypeExtension(InputObjectTypeExtension),
}

struct ScalarTypeExtension {
    loc: Option<Location>,
    name: Name,
    directives: Option<Vec<Directive>>,
}

struct ObjectTypeExtension {
    loc: Option<Location>,
    name: Name,
    interfaces: Option<Vec<NamedType>>,
    directives: Option<Vec<Directive>>,
    fields: Option<Vec<FieldDefinition>>,
}

struct InterfaceTypeExtension {
    loc: Option<Location>,
    name: Name,
    directives: Option<Vec<Directive>>,
    fields: Option<Vec<FieldDefinition>>,
}

struct UnionTypeExtension {
    loc: Option<Location>,
    name: Name,
    directives: Option<Vec<Directive>>,
    types: Option<Vec<NamedType>>,
}

struct EnumTypeExtension {
    loc: Option<Location>,
    name: Name,
    directives: Option<Vec<Directive>>,
    values: Option<Vec<EnumValueDefinition>>,
}

struct InputObjectTypeExtension {
    loc: Option<Location>,
    name: Name,
    directives: Option<Vec<Directive>>,
    fields: Option<Vec<InputValueDefinition>>,
}
