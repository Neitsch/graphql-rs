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

/**
 * The list of all possible AST node types.
 */
#[derive(Debug, Clone, PartialEq)]
pub enum AST<'a> {
    Name(&'a Name),
    Document(&'a Document),
    OperationDefinition(&'a OperationDefinition),
    VariableDefinition(&'a VariableDefinition),
    Variable(&'a Variable),
    SelectionSet(&'a SelectionSet),
    Field(&'a Field),
    Argument(&'a Argument),
    FragmentSpread(&'a FragmentSpread),
    InlineFragment(&'a InlineFragment),
    FragmentDefinition(&'a FragmentDefinition),
    IntValue(&'a IntValue),
    FloatValue(&'a FloatValue),
    StringValue(&'a StringValue),
    BooleanValue(&'a BooleanValue),
    NullValue(&'a NullValue),
    EnumValue(&'a EnumValue),
    ListValue(&'a ListValue),
    ObjectValue(&'a ObjectValue),
    ObjectField(&'a ObjectField),
    Directive(&'a Directive),
    NamedType(&'a NamedType),
    ListType(&'a ListType),
    NonNullType(&'a NonNullType),
    SchemaDefinition(&'a SchemaDefinition),
    OperationTypeDefinition(&'a OperationTypeDefinition),
    ScalarTypeDefinition(&'a ScalarTypeDefinition),
    ObjectTypeDefinition(&'a ObjectTypeDefinition),
    FieldDefinition(&'a FieldDefinition),
    InputValueDefinition(&'a InputValueDefinition),
    InterfaceTypeDefinition(&'a InterfaceTypeDefinition),
    UnionTypeDefinition(&'a UnionTypeDefinition),
    EnumTypeDefinition(&'a EnumTypeDefinition),
    EnumValueDefinition(&'a EnumValueDefinition),
    InputObjectTypeDefinition(&'a InputObjectTypeDefinition),
    DirectiveDefinition(&'a DirectiveDefinition),
    SchemaExtension(&'a SchemaExtension),
    ScalarTypeExtension(&'a ScalarTypeExtension),
    ObjectTypeExtension(&'a ObjectTypeExtension),
    InterfaceTypeExtension(&'a InterfaceTypeExtension),
    UnionTypeExtension(&'a UnionTypeExtension),
    EnumTypeExtension(&'a EnumTypeExtension),
    InputObjectTypeExtension(&'a InputObjectTypeExtension),
}

impl<'a> fmt::Display for AST<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AST::Name(val) => write!(f, "{}", val),
            AST::Document(val) => write!(f, "{}", val),
            AST::OperationDefinition(val) => write!(f, "{}", val),
            AST::VariableDefinition(val) => write!(f, "{}", val),
            AST::Variable(val) => write!(f, "{}", val),
            AST::SelectionSet(val) => write!(f, "{}", val),
            AST::Field(val) => write!(f, "{}", val),
            AST::Argument(val) => write!(f, "{}", val),
            AST::FragmentSpread(val) => write!(f, "{}", val),
            AST::InlineFragment(val) => write!(f, "{}", val),
            AST::FragmentDefinition(val) => write!(f, "{}", val),
            AST::IntValue(val) => write!(f, "{}", val),
            AST::FloatValue(val) => write!(f, "{}", val),
            AST::StringValue(val) => write!(f, "{}", val),
            AST::BooleanValue(val) => write!(f, "{}", val),
            AST::NullValue(val) => write!(f, "{}", val),
            AST::EnumValue(val) => write!(f, "{}", val),
            AST::ListValue(val) => write!(f, "{}", val),
            AST::ObjectValue(val) => write!(f, "{}", val),
            AST::ObjectField(val) => write!(f, "{}", val),
            AST::Directive(val) => write!(f, "{}", val),
            AST::NamedType(val) => write!(f, "{}", val),
            AST::ListType(val) => write!(f, "{}", val),
            AST::NonNullType(val) => write!(f, "{}", val),
            AST::SchemaDefinition(val) => write!(f, "{}", val),
            AST::OperationTypeDefinition(val) => write!(f, "{}", val),
            AST::ScalarTypeDefinition(val) => write!(f, "{}", val),
            AST::ObjectTypeDefinition(val) => write!(f, "{}", val),
            AST::FieldDefinition(val) => write!(f, "{}", val),
            AST::InputValueDefinition(val) => write!(f, "{}", val),
            AST::InterfaceTypeDefinition(val) => write!(f, "{}", val),
            AST::UnionTypeDefinition(val) => write!(f, "{}", val),
            AST::EnumTypeDefinition(val) => write!(f, "{}", val),
            AST::EnumValueDefinition(val) => write!(f, "{}", val),
            AST::InputObjectTypeDefinition(val) => write!(f, "{}", val),
            AST::DirectiveDefinition(val) => write!(f, "{}", val),
            AST::SchemaExtension(val) => write!(f, "{}", val),
            AST::ScalarTypeExtension(val) => write!(f, "{}", val),
            AST::ObjectTypeExtension(val) => write!(f, "{}", val),
            AST::InterfaceTypeExtension(val) => write!(f, "{}", val),
            AST::UnionTypeExtension(val) => write!(f, "{}", val),
            AST::EnumTypeExtension(val) => write!(f, "{}", val),
            AST::InputObjectTypeExtension(val) => write!(f, "{}", val),
        }
    }
}

// Name
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Name {
    loc: Option<Location>,
    pub value: String,
}

impl Name {
    pub fn new(name: String, loc: Location) -> Name {
        Name {
            loc: Some(loc),
            value: name,
        }
    }
}

impl<'a> From<&'a Name> for AST<'a> {
    fn from(name: &'a Name) -> Self {
        AST::Name(name)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptName(pub Option<Name>);

impl From<Option<Name>> for OptName {
    fn from(d: Option<Name>) -> Self {
        OptName(d)
    }
}

impl fmt::Display for OptName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptName(Some(name)) = &self {
            write!(f, "{}", name)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptName {
    type Target = Option<Name>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptName {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

// Document
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Document {
    pub loc: Option<Location>,
    pub definitions: DefinitionVec,
}

impl<'a> From<&'a Document> for AST<'a> {
    fn from(doc: &'a Document) -> Self {
        AST::Document(doc)
    }
}

impl fmt::Display for Document {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.definitions,)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Definition {
    ExecutableDefinition(Box<ExecutableDefinition>),
    TypeSystemDefinition(Box<TypeSystemDefinition>),
    TypeSystemExtension(Box<TypeSystemExtension>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefinitionVec(pub Vec<Definition>);

impl<'a> From<&'a Definition> for AST<'a> {
    fn from(def: &'a Definition) -> Self {
        match def {
            Definition::ExecutableDefinition(ed) => AST::from(ed),
            Definition::TypeSystemDefinition(tsd) => AST::from(tsd),
            Definition::TypeSystemExtension(tse) => AST::from(tse),
        }
    }
}

impl From<Vec<Definition>> for DefinitionVec {
    fn from(d: Vec<Definition>) -> Self {
        DefinitionVec(d)
    }
}

impl fmt::Display for Definition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Definition::TypeSystemDefinition(tsd) => write!(f, "{}", tsd),
            Definition::ExecutableDefinition(ed) => write!(f, "{}", ed),
            Definition::TypeSystemExtension(tse) => write!(f, "{}", tse),
        }
    }
}

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

impl core::ops::Deref for DefinitionVec {
    type Target = Vec<Definition>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for DefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

impl From<ExecutableDefinition> for Definition {
    fn from(d: ExecutableDefinition) -> Self {
        Definition::ExecutableDefinition(Box::new(d))
    }
}
impl From<TypeSystemDefinition> for Definition {
    fn from(d: TypeSystemDefinition) -> Self {
        Definition::TypeSystemDefinition(Box::new(d))
    }
}
impl From<TypeSystemExtension> for Definition {
    fn from(d: TypeSystemExtension) -> Self {
        Definition::TypeSystemExtension(Box::new(d))
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExecutableDefinition {
    OperationDefinition(Box<OperationDefinition>),
    FragmentDefinition(Box<FragmentDefinition>),
}

impl<'a> From<&'a ExecutableDefinition> for AST<'a> {
    fn from(ex: &'a ExecutableDefinition) -> Self {
        match ex {
            ExecutableDefinition::OperationDefinition(op) => AST::from(op),
            ExecutableDefinition::FragmentDefinition(fd) => AST::from(fd),
        }
    }
}

impl fmt::Display for ExecutableDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExecutableDefinition::OperationDefinition(od) => write!(f, "{}", od),
            ExecutableDefinition::FragmentDefinition(fd) => write!(f, "{}", fd),
        }
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperationDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    pub name: OptName,
    #[serde(rename = "variableDefinitions")]
    pub variable_definitions: OptVariableDefinitionVec,
    pub directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    pub selection_set: SelectionSet,
}

impl<'a> From<&'a OperationDefinition> for AST<'a> {
    fn from(od: &'a OperationDefinition) -> Self {
        AST::OperationDefinition(od)
    }
}

impl fmt::Display for OperationDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}{}{} {}",
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

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDefinition {
    pub loc: Option<Location>,
    pub variable: Variable,
    #[serde(rename = "type")]
    pub _type: Type,
    #[serde(rename = "defaultValue")]
    pub default_value: Option<Value>,
    pub directives: OptDirectiveVec,
}

impl<'a> From<&'a VariableDefinition> for AST<'a> {
    fn from(v: &'a VariableDefinition) -> Self {
        AST::VariableDefinition(v)
    }
}

impl fmt::Display for VariableDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.variable, self._type)?;
        if let Some(dv) = &self.default_value {
            write!(f, " = {}", dv)?;
        }
        if let OptDirectiveVec(Some(d)) = &self.directives {
            write!(f, " {}", d)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableDefinitionVec(pub Vec<VariableDefinition>);

impl From<Vec<VariableDefinition>> for VariableDefinitionVec {
    fn from(d: Vec<VariableDefinition>) -> Self {
        VariableDefinitionVec(d)
    }
}

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

impl core::ops::Deref for VariableDefinitionVec {
    type Target = Vec<VariableDefinition>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for VariableDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptVariableDefinitionVec(pub Option<VariableDefinitionVec>);

impl From<Option<VariableDefinitionVec>> for OptVariableDefinitionVec {
    fn from(d: Option<VariableDefinitionVec>) -> Self {
        OptVariableDefinitionVec(d)
    }
}

impl fmt::Display for OptVariableDefinitionVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptVariableDefinitionVec(Some(val)) = &self {
            write!(f, "({})", val)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptVariableDefinitionVec {
    type Target = Option<VariableDefinitionVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptVariableDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variable {
    pub loc: Option<Location>,
    pub name: Name,
}

impl<'a> From<&'a Variable> for AST<'a> {
    fn from(v: &'a Variable) -> Self {
        AST::Variable(v)
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.name)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SelectionSet {
    pub loc: Option<Location>,
    pub selections: Vec<Selection>,
}

impl<'a> From<&'a SelectionSet> for AST<'a> {
    fn from(s: &'a SelectionSet) -> Self {
        AST::SelectionSet(s)
    }
}

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

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Selection {
    Field(Box<Field>),
    FragmentSpread(Box<FragmentSpread>),
    InlineFragment(Box<InlineFragment>),
}

impl<'a> From<&'a Selection> for AST<'a> {
    fn from(s: &'a Selection) -> Self {
        match s {
            Selection::Field(f) => AST::from(f),
            Selection::FragmentSpread(fs) => AST::from(fs),
            Selection::InlineFragment(f) => AST::from(f),
        }
    }
}

impl fmt::Display for Selection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Selection::Field(field) => write!(f, "{}", field),
            Selection::FragmentSpread(fs) => write!(f, "{}", fs),
            Selection::InlineFragment(frag) => write!(f, "{}", frag),
        }
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub loc: Option<Location>,
    pub alias: OptName,
    pub name: Name,
    pub arguments: OptArgumentVec,
    pub directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    pub selection_set: Option<SelectionSet>,
}

impl<'a> From<&'a Field> for AST<'a> {
    fn from(s: &'a Field) -> Self {
        AST::Field(s)
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptName(Some(name)) = &self.alias {
            write!(f, "{}: ", name,)?;
        }
        write!(f, "{}{}{}", self.name, self.arguments, self.directives)?;
        if let Some(sel) = &self.selection_set {
            write!(f, " {}", sel)?;
        }
        Ok(())
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Argument {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

impl<'a> From<&'a Argument> for AST<'a> {
    fn from(s: &'a Argument) -> Self {
        AST::Argument(s)
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArgumentVec(pub Vec<Argument>);

impl From<Vec<Argument>> for ArgumentVec {
    fn from(d: Vec<Argument>) -> Self {
        ArgumentVec(d)
    }
}

impl fmt::Display for ArgumentVec {
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

impl core::ops::Deref for ArgumentVec {
    type Target = Vec<Argument>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for ArgumentVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptArgumentVec(pub Option<ArgumentVec>);

impl From<Option<Vec<Argument>>> for OptArgumentVec {
    fn from(d: Option<Vec<Argument>>) -> Self {
        OptArgumentVec(d.map(std::convert::Into::into))
    }
}

impl fmt::Display for OptArgumentVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({})",
            self.iter()
                .map(|vd| format!("{}", vd))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

impl core::ops::Deref for OptArgumentVec {
    type Target = Option<ArgumentVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptArgumentVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

// Fragments
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FragmentSpread {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: OptDirectiveVec,
}

impl<'a> From<&'a FragmentSpread> for AST<'a> {
    fn from(s: &'a FragmentSpread) -> Self {
        AST::FragmentSpread(s)
    }
}

impl fmt::Display for FragmentSpread {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...{}{}", self.name, self.directives)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InlineFragment {
    pub loc: Option<Location>,
    #[serde(rename = "typeCondition")]
    pub type_condition: Option<NamedType>,
    pub directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    pub selection_set: SelectionSet,
}

impl<'a> From<&'a InlineFragment> for AST<'a> {
    fn from(s: &'a InlineFragment) -> Self {
        AST::InlineFragment(s)
    }
}

impl fmt::Display for InlineFragment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "...")?;
        if let Some(tc) = &self.type_condition {
            write!(f, " on {}", tc)?;
        }
        write!(f, "{} {}", self.directives, self.selection_set)?;
        Ok(())
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FragmentDefinition {
    pub loc: Option<Location>,
    pub name: Name,
    // Note: fragment variable definitions are experimental and may be changed
    // or removed in the future.
    #[serde(rename = "variableDefinitions")]
    pub variable_definitions: OptVariableDefinitionVec,
    #[serde(rename = "typeCondition")]
    pub type_condition: NamedType,
    pub directives: OptDirectiveVec,
    #[serde(rename = "selectionSet")]
    pub selection_set: SelectionSet,
}

impl<'a> From<&'a FragmentDefinition> for AST<'a> {
    fn from(s: &'a FragmentDefinition) -> Self {
        AST::FragmentDefinition(s)
    }
}

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

impl<'a> From<&'a Value> for AST<'a> {
    fn from(v: &'a Value) -> Self {
        match v {
            Value::Variable(var) => AST::from(var),
            Value::IntValue(var) => AST::from(var),
            Value::FloatValue(var) => AST::from(var),
            Value::StringValue(var) => AST::from(var),
            Value::BooleanValue(var) => AST::from(var),
            Value::NullValue(var) => AST::from(var),
            Value::EnumValue(var) => AST::from(var),
            Value::ListValue(var) => AST::from(var),
            Value::ObjectValue(var) => AST::from(var),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Variable(v) => write!(f, "{}", v),
            Value::IntValue(v) => write!(f, "{}", v),
            Value::FloatValue(v) => write!(f, "{}", v),
            Value::StringValue(v) => write!(f, "{}", v),
            Value::BooleanValue(v) => write!(f, "{}", v),
            Value::NullValue(v) => write!(f, "{}", v),
            Value::EnumValue(v) => write!(f, "{}", v),
            Value::ListValue(v) => write!(f, "{}", v),
            Value::ObjectValue(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptValue(pub Option<Value>);

impl From<Option<Value>> for OptValue {
    fn from(d: Option<Value>) -> Self {
        OptValue(d)
    }
}

impl fmt::Display for OptValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptValue(Some(val)) = self {
            write!(f, " = {}", val)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptValue {
    type Target = Option<Value>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptValue {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValueVec(pub Vec<Value>);

impl From<Vec<Value>> for ValueVec {
    fn from(d: Vec<Value>) -> Self {
        ValueVec(d)
    }
}

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

impl core::ops::Deref for ValueVec {
    type Target = Vec<Value>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for ValueVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IntValue {
    pub loc: Option<Location>,
    pub value: String,
}

impl<'a> From<&'a IntValue> for AST<'a> {
    fn from(s: &'a IntValue) -> Self {
        AST::IntValue(s)
    }
}

impl fmt::Display for IntValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FloatValue {
    pub loc: Option<Location>,
    pub value: String,
}

impl<'a> From<&'a FloatValue> for AST<'a> {
    fn from(s: &'a FloatValue) -> Self {
        AST::FloatValue(s)
    }
}

impl fmt::Display for FloatValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StringValue {
    pub loc: Option<Location>,
    pub value: String,
    pub block: Option<bool>,
}

impl<'a> From<&'a StringValue> for AST<'a> {
    fn from(s: &'a StringValue) -> Self {
        AST::StringValue(s)
    }
}

impl fmt::Display for StringValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let delimiter = match &self.block {
            Some(true) => "\"\"\"",
            _ => "\"",
        };
        write!(f, "{}{}{}", delimiter, self.value, delimiter)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Description(pub Option<StringValue>);

impl From<Option<StringValue>> for Description {
    fn from(d: Option<StringValue>) -> Self {
        Description(d)
    }
}

impl fmt::Display for Description {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Description(Some(val)) = self {
            write!(f, "{}", val)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for Description {
    type Target = Option<StringValue>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for Description {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BooleanValue {
    pub loc: Option<Location>,
    pub value: bool,
}

impl<'a> From<&'a BooleanValue> for AST<'a> {
    fn from(s: &'a BooleanValue) -> Self {
        AST::BooleanValue(s)
    }
}

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

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct NullValue {
    pub loc: Option<Location>,
}

impl fmt::Display for NullValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "NULL")
    }
}

impl<'a> From<&'a NullValue> for AST<'a> {
    fn from(s: &'a NullValue) -> Self {
        AST::NullValue(s)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumValue {
    pub loc: Option<Location>,
    pub value: String,
}

impl<'a> From<&'a EnumValue> for AST<'a> {
    fn from(s: &'a EnumValue) -> Self {
        AST::EnumValue(s)
    }
}

impl fmt::Display for EnumValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListValue {
    pub loc: Option<Location>,
    pub values: ValueVec,
}

impl<'a> From<&'a ListValue> for AST<'a> {
    fn from(s: &'a ListValue) -> Self {
        AST::ListValue(s)
    }
}

impl fmt::Display for ListValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.values)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectValue {
    pub loc: Option<Location>,
    pub fields: ObjectFieldVec,
}

impl<'a> From<&'a ObjectValue> for AST<'a> {
    fn from(s: &'a ObjectValue) -> Self {
        AST::ObjectValue(s)
    }
}

impl fmt::Display for ObjectValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.fields)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectField {
    pub loc: Option<Location>,
    pub name: Name,
    pub value: Value,
}

impl<'a> From<&'a ObjectField> for AST<'a> {
    fn from(s: &'a ObjectField) -> Self {
        AST::ObjectField(s)
    }
}

impl fmt::Display for ObjectField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectFieldVec(pub Vec<ObjectField>);

impl From<Vec<ObjectField>> for ObjectFieldVec {
    fn from(d: Vec<ObjectField>) -> Self {
        ObjectFieldVec(d)
    }
}

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

impl core::ops::Deref for ObjectFieldVec {
    type Target = Vec<ObjectField>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for ObjectFieldVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

// Directives
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Directive {
    pub loc: Option<Location>,
    pub name: Name,
    pub arguments: OptArgumentVec,
}

impl<'a> From<&'a Directive> for AST<'a> {
    fn from(s: &'a Directive) -> Self {
        AST::Directive(s)
    }
}

impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)?;
        if let OptArgumentVec(Some(args)) = &self.arguments {
            write!(f, "{}", args)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DirectiveVec(pub Vec<Directive>);

impl From<Vec<Directive>> for DirectiveVec {
    fn from(d: Vec<Directive>) -> Self {
        DirectiveVec(d)
    }
}

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

impl core::ops::Deref for DirectiveVec {
    type Target = Vec<Directive>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for DirectiveVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptDirectiveVec(pub Option<DirectiveVec>);

impl From<Option<Vec<Directive>>> for OptDirectiveVec {
    fn from(d: Option<Vec<Directive>>) -> Self {
        OptDirectiveVec(d.map(std::convert::Into::into))
    }
}

impl fmt::Display for OptDirectiveVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptDirectiveVec(Some(dir)) = self {
            write!(f, " {}", dir)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptDirectiveVec {
    type Target = Option<DirectiveVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptDirectiveVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

// Type Reference
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    NamedType(Box<NamedType>),
    ListType(Box<ListType>),
    NonNullType(Box<NonNullType>),
}

impl<'a> From<&'a Type> for AST<'a> {
    fn from(t: &'a Type) -> Self {
        match t {
            Type::NamedType(nt) => AST::from(nt),
            Type::ListType(nt) => AST::from(nt),
            Type::NonNullType(nt) => AST::from(nt),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::NamedType(t) => write!(f, "{}", t),
            Type::ListType(t) => write!(f, "{}", t),
            Type::NonNullType(t) => write!(f, "{}", t),
        }
    }
}

impl From<NamedType> for Type {
    fn from(t: NamedType) -> Self {
        Type::NamedType(Box::new(t))
    }
}
impl From<ListType> for Type {
    fn from(t: ListType) -> Self {
        Type::ListType(Box::new(t))
    }
}
impl From<NonNullType> for Type {
    fn from(t: NonNullType) -> Self {
        Type::NonNullType(Box::new(t))
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NamedType {
    pub loc: Option<Location>,
    pub name: Name,
}

impl<'a> From<&'a NamedType> for AST<'a> {
    fn from(s: &'a NamedType) -> Self {
        AST::NamedType(s)
    }
}

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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
struct OptNamedType(Option<NamedType>);

impl From<Option<NamedType>> for OptNamedType {
    fn from(d: Option<NamedType>) -> Self {
        OptNamedType(d)
    }
}

impl fmt::Display for OptNamedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptNamedType(Some(t)) = self {
            write!(f, "{}", t)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptNamedType {
    type Target = Option<NamedType>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptNamedType {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ListType {
    pub loc: Option<Location>,
    #[serde(rename = "type")]
    pub _type: Box<Type>,
}

impl<'a> From<&'a ListType> for AST<'a> {
    fn from(s: &'a ListType) -> Self {
        AST::ListType(s)
    }
}

impl fmt::Display for ListType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self._type)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NonNullInnerType {
    NamedType(Box<NamedType>),
    ListType(Box<ListType>),
}

impl<'a> From<&'a NonNullInnerType> for AST<'a> {
    fn from(nnt: &'a NonNullInnerType) -> Self {
        match &nnt {
            NonNullInnerType::NamedType(nt) => AST::from(nt),
            NonNullInnerType::ListType(lt) => AST::from(lt),
        }
    }
}

impl fmt::Display for NonNullInnerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NonNullInnerType::NamedType(nt) => write!(f, "{}", nt),
            NonNullInnerType::ListType(lt) => write!(f, "{}", lt),
        }
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NonNullType {
    pub loc: Option<Location>,
    #[serde(rename = "type")]
    pub _type: NonNullInnerType,
}

impl<'a> From<&'a NonNullType> for AST<'a> {
    fn from(s: &'a NonNullType) -> Self {
        AST::NonNullType(s)
    }
}

impl fmt::Display for NonNullType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}!", self._type)
    }
}

// Type System Definition
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSystemDefinition {
    SchemaDefinition(Box<SchemaDefinition>),
    TypeDefinition(Box<TypeDefinition>),
    DirectiveDefinition(Box<DirectiveDefinition>),
}

impl<'a> From<&'a TypeSystemDefinition> for AST<'a> {
    fn from(s: &'a TypeSystemDefinition) -> Self {
        match s {
            TypeSystemDefinition::SchemaDefinition(sd) => AST::from(sd),
            TypeSystemDefinition::TypeDefinition(td) => AST::from(td),
            TypeSystemDefinition::DirectiveDefinition(dd) => AST::from(dd),
        }
    }
}

impl fmt::Display for TypeSystemDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeSystemDefinition::SchemaDefinition(sd) => write!(f, "{}", sd),
            TypeSystemDefinition::TypeDefinition(td) => write!(f, "{}", td),
            TypeSystemDefinition::DirectiveDefinition(dd) => write!(f, "{}", dd),
        }
    }
}

impl From<SchemaDefinition> for TypeSystemDefinition {
    fn from(s: SchemaDefinition) -> Self {
        TypeSystemDefinition::SchemaDefinition(Box::new(s))
    }
}

impl From<TypeDefinition> for TypeSystemDefinition {
    fn from(s: TypeDefinition) -> Self {
        TypeSystemDefinition::TypeDefinition(Box::new(s))
    }
}

impl From<DirectiveDefinition> for TypeSystemDefinition {
    fn from(s: DirectiveDefinition) -> Self {
        TypeSystemDefinition::DirectiveDefinition(Box::new(s))
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SchemaDefinition {
    pub loc: Option<Location>,
    pub directives: OptDirectiveVec,
    #[serde(rename = "operationTypes")]
    pub operation_types: OperationTypeDefinitionVec,
}

impl<'a> From<&'a SchemaDefinition> for AST<'a> {
    fn from(s: &'a SchemaDefinition) -> Self {
        AST::SchemaDefinition(s)
    }
}

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

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperationTypeDefinition {
    pub loc: Option<Location>,
    pub operation: OperationType,
    #[serde(rename = "type")]
    pub _type: NamedType,
}

impl<'a> From<&'a OperationTypeDefinition> for AST<'a> {
    fn from(s: &'a OperationTypeDefinition) -> Self {
        AST::OperationTypeDefinition(s)
    }
}

impl fmt::Display for OperationTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.operation, self._type)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperationTypeDefinitionVec(pub Vec<OperationTypeDefinition>);

impl From<Vec<OperationTypeDefinition>> for OperationTypeDefinitionVec {
    fn from(d: Vec<OperationTypeDefinition>) -> Self {
        OperationTypeDefinitionVec(d)
    }
}

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

impl core::ops::Deref for OperationTypeDefinitionVec {
    type Target = Vec<OperationTypeDefinition>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OperationTypeDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptOperationTypeDefinitionVec(pub Option<OperationTypeDefinitionVec>);

impl From<Option<Vec<OperationTypeDefinition>>> for OptOperationTypeDefinitionVec {
    fn from(d: Option<Vec<OperationTypeDefinition>>) -> Self {
        OptOperationTypeDefinitionVec(d.map(std::convert::Into::into))
    }
}

impl fmt::Display for OptOperationTypeDefinitionVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptOperationTypeDefinitionVec(Some(otd)) = self {
            write!(f, "{}", otd)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptOperationTypeDefinitionVec {
    type Target = Option<OperationTypeDefinitionVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptOperationTypeDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
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

impl From<ScalarTypeDefinition> for TypeDefinition {
    fn from(d: ScalarTypeDefinition) -> Self {
        TypeDefinition::ScalarTypeDefinition(Box::new(d))
    }
}

impl From<ObjectTypeDefinition> for TypeDefinition {
    fn from(d: ObjectTypeDefinition) -> Self {
        TypeDefinition::ObjectTypeDefinition(Box::new(d))
    }
}

impl From<InterfaceTypeDefinition> for TypeDefinition {
    fn from(d: InterfaceTypeDefinition) -> Self {
        TypeDefinition::InterfaceTypeDefinition(Box::new(d))
    }
}

impl From<UnionTypeDefinition> for TypeDefinition {
    fn from(d: UnionTypeDefinition) -> Self {
        TypeDefinition::UnionTypeDefinition(Box::new(d))
    }
}

impl From<EnumTypeDefinition> for TypeDefinition {
    fn from(d: EnumTypeDefinition) -> Self {
        TypeDefinition::EnumTypeDefinition(Box::new(d))
    }
}

impl From<InputObjectTypeDefinition> for TypeDefinition {
    fn from(d: InputObjectTypeDefinition) -> Self {
        TypeDefinition::InputObjectTypeDefinition(Box::new(d))
    }
}

impl<'a> From<&'a TypeDefinition> for AST<'a> {
    fn from(s: &'a TypeDefinition) -> Self {
        match s {
            TypeDefinition::ScalarTypeDefinition(std) => AST::from(std),
            TypeDefinition::ObjectTypeDefinition(std) => AST::from(std),
            TypeDefinition::InterfaceTypeDefinition(std) => AST::from(std),
            TypeDefinition::UnionTypeDefinition(std) => AST::from(std),
            TypeDefinition::EnumTypeDefinition(std) => AST::from(std),
            TypeDefinition::InputObjectTypeDefinition(std) => AST::from(std),
        }
    }
}

impl fmt::Display for TypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeDefinition::ScalarTypeDefinition(std) => write!(f, "{}", std),
            TypeDefinition::ObjectTypeDefinition(std) => write!(f, "{}", std),
            TypeDefinition::InterfaceTypeDefinition(std) => write!(f, "{}", std),
            TypeDefinition::UnionTypeDefinition(std) => write!(f, "{}", std),
            TypeDefinition::EnumTypeDefinition(std) => write!(f, "{}", std),
            TypeDefinition::InputObjectTypeDefinition(std) => write!(f, "{}", std),
        }
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ScalarTypeDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub directives: OptDirectiveVec,
}

impl fmt::Display for ScalarTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}scalar {}{}",
            self.description, self.name, self.directives
        )
    }
}

impl<'a> From<&'a ScalarTypeDefinition> for AST<'a> {
    fn from(s: &'a ScalarTypeDefinition) -> Self {
        AST::ScalarTypeDefinition(s)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: OptDirectiveVec,
    pub fields: OptFieldDefinitionVec,
}

impl<'a> From<&'a ObjectTypeDefinition> for AST<'a> {
    fn from(s: &'a ObjectTypeDefinition) -> Self {
        AST::ObjectTypeDefinition(s)
    }
}

impl fmt::Display for ObjectTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let interface_data = if let Some(itf) = &self.interfaces {
            itf.iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join(" & ")
        } else {
            "".to_string()
        };
        write!(
            f,
            "{}type {}{}{}{}",
            self.description, self.name, interface_data, self.directives, self.fields
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub arguments: OptInputValueDefinitionVec,
    #[serde(rename = "type")]
    pub _type: Type,
    pub directives: OptDirectiveVec,
}

impl<'a> From<&'a FieldDefinition> for AST<'a> {
    fn from(s: &'a FieldDefinition) -> Self {
        AST::FieldDefinition(s)
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}: {}{}",
            self.description, self.name, self.arguments, self._type, self.directives
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldDefinitionVec(pub Vec<FieldDefinition>);

impl From<Vec<FieldDefinition>> for FieldDefinitionVec {
    fn from(d: Vec<FieldDefinition>) -> Self {
        FieldDefinitionVec(d)
    }
}

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

impl core::ops::Deref for FieldDefinitionVec {
    type Target = Vec<FieldDefinition>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for FieldDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct OptFieldDefinitionVec(pub Option<FieldDefinitionVec>);

impl From<Option<Vec<FieldDefinition>>> for OptFieldDefinitionVec {
    fn from(d: Option<Vec<FieldDefinition>>) -> Self {
        OptFieldDefinitionVec(d.map(std::convert::Into::into))
    }
}

impl fmt::Display for OptFieldDefinitionVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptFieldDefinitionVec(Some(ivd)) = self {
            write!(f, "{}", ivd)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptFieldDefinitionVec {
    type Target = Option<FieldDefinitionVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptFieldDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<FieldDefinition>> for OptFieldDefinitionVec {
    fn from(v: Vec<FieldDefinition>) -> Self {
        Some(v).into()
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputValueDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    #[serde(rename = "type")]
    pub _type: Type,
    #[serde(rename = "defaultValue")]
    pub default_value: OptValue,
    pub directives: OptDirectiveVec,
}

impl<'a> From<&'a InputValueDefinition> for AST<'a> {
    fn from(s: &'a InputValueDefinition) -> Self {
        AST::InputValueDefinition(s)
    }
}

impl fmt::Display for InputValueDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}: {}{}{}",
            self.description, self.name, self._type, self.default_value, self.directives
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputValueDefinitionVec(pub Vec<InputValueDefinition>);

impl From<Vec<InputValueDefinition>> for InputValueDefinitionVec {
    fn from(d: Vec<InputValueDefinition>) -> Self {
        InputValueDefinitionVec(d)
    }
}

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

impl core::ops::Deref for InputValueDefinitionVec {
    type Target = Vec<InputValueDefinition>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for InputValueDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptInputValueDefinitionVec(pub Option<InputValueDefinitionVec>);

impl From<Option<Vec<InputValueDefinition>>> for OptInputValueDefinitionVec {
    fn from(d: Option<Vec<InputValueDefinition>>) -> Self {
        OptInputValueDefinitionVec(d.map(std::convert::Into::into))
    }
}

impl fmt::Display for OptInputValueDefinitionVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptInputValueDefinitionVec(Some(ivd)) = self {
            write!(f, "{}", ivd)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptInputValueDefinitionVec {
    type Target = Option<InputValueDefinitionVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptInputValueDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceTypeDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub directives: OptDirectiveVec,
    pub fields: OptFieldDefinitionVec,
}

impl<'a> From<&'a InterfaceTypeDefinition> for AST<'a> {
    fn from(s: &'a InterfaceTypeDefinition) -> Self {
        AST::InterfaceTypeDefinition(s)
    }
}

impl fmt::Display for InterfaceTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}interface {}{}{}",
            self.description, self.name, self.directives, self.fields
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnionTypeDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub directives: OptDirectiveVec,
    pub types: Option<Vec<NamedType>>,
}

impl<'a> From<&'a UnionTypeDefinition> for AST<'a> {
    fn from(s: &'a UnionTypeDefinition) -> Self {
        AST::UnionTypeDefinition(s)
    }
}

impl fmt::Display for UnionTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let union_types = if let Some(t) = &self.types {
            t.iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join(" | ")
        } else {
            "".to_string()
        };
        write!(
            f,
            "{}union {}{}{}",
            self.description, self.name, self.directives, union_types
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumTypeDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub directives: OptDirectiveVec,
    pub values: OptEnumValueDefinitionVec,
}

impl<'a> From<&'a EnumTypeDefinition> for AST<'a> {
    fn from(s: &'a EnumTypeDefinition) -> Self {
        AST::EnumTypeDefinition(s)
    }
}

impl fmt::Display for EnumTypeDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}enum {}{}{}",
            self.description, self.name, self.directives, self.values
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumValueDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub directives: OptDirectiveVec,
}

impl<'a> From<&'a EnumValueDefinition> for AST<'a> {
    fn from(s: &'a EnumValueDefinition) -> Self {
        AST::EnumValueDefinition(s)
    }
}

impl fmt::Display for EnumValueDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.description, self.name, self.directives)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumValueDefinitionVec(pub Vec<EnumValueDefinition>);

impl From<Vec<EnumValueDefinition>> for EnumValueDefinitionVec {
    fn from(d: Vec<EnumValueDefinition>) -> Self {
        EnumValueDefinitionVec(d)
    }
}

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

impl core::ops::Deref for EnumValueDefinitionVec {
    type Target = Vec<EnumValueDefinition>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for EnumValueDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OptEnumValueDefinitionVec(pub Option<EnumValueDefinitionVec>);

impl From<Option<Vec<EnumValueDefinition>>> for OptEnumValueDefinitionVec {
    fn from(d: Option<Vec<EnumValueDefinition>>) -> Self {
        OptEnumValueDefinitionVec(d.map(std::convert::Into::into))
    }
}

impl fmt::Display for OptEnumValueDefinitionVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let OptEnumValueDefinitionVec(Some(ivd)) = self {
            write!(f, "{}", ivd)?;
        }
        Ok(())
    }
}

impl core::ops::Deref for OptEnumValueDefinitionVec {
    type Target = Option<EnumValueDefinitionVec>;

    fn deref(self: &'_ Self) -> &'_ Self::Target {
        &self.0
    }
}

impl core::ops::DerefMut for OptEnumValueDefinitionVec {
    fn deref_mut(self: &'_ mut Self) -> &'_ mut Self::Target {
        &mut self.0
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputObjectTypeDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub directives: OptDirectiveVec,
    pub fields: OptInputValueDefinitionVec,
}

impl<'a> From<&'a InputObjectTypeDefinition> for AST<'a> {
    fn from(s: &'a InputObjectTypeDefinition) -> Self {
        AST::InputObjectTypeDefinition(s)
    }
}

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
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DirectiveDefinition {
    pub loc: Option<Location>,
    pub description: Description,
    pub name: Name,
    pub arguments: OptInputValueDefinitionVec,
    pub locations: Vec<Name>,
}

impl<'a> From<&'a DirectiveDefinition> for AST<'a> {
    fn from(s: &'a DirectiveDefinition) -> Self {
        AST::DirectiveDefinition(s)
    }
}

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
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeSystemExtension {
    SchemaExtension(Box<SchemaExtension>),
    TypeExtension(Box<TypeExtension>),
}

impl<'a> From<&'a TypeSystemExtension> for AST<'a> {
    fn from(s: &'a TypeSystemExtension) -> Self {
        match s {
            TypeSystemExtension::SchemaExtension(se) => AST::from(se),
            TypeSystemExtension::TypeExtension(te) => AST::from(te),
        }
    }
}

impl fmt::Display for TypeSystemExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeSystemExtension::SchemaExtension(se) => write!(f, "{}", se),
            TypeSystemExtension::TypeExtension(te) => write!(f, "{}", te),
        }
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SchemaExtension {
    pub loc: Option<Location>,
    pub directives: OptDirectiveVec,
    #[serde(rename = "operationTypes")]
    pub operation_types: OptOperationTypeDefinitionVec,
}

impl<'a> From<&'a SchemaExtension> for AST<'a> {
    fn from(s: &'a SchemaExtension) -> Self {
        AST::SchemaExtension(s)
    }
}

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

impl<'a> From<&'a TypeExtension> for AST<'a> {
    fn from(s: &'a TypeExtension) -> Self {
        match s {
            TypeExtension::ScalarTypeExtension(v) => AST::from(v),
            TypeExtension::ObjectTypeExtension(v) => AST::from(v),
            TypeExtension::InterfaceTypeExtension(v) => AST::from(v),
            TypeExtension::UnionTypeExtension(v) => AST::from(v),
            TypeExtension::EnumTypeExtension(v) => AST::from(v),
            TypeExtension::InputObjectTypeExtension(v) => AST::from(v),
        }
    }
}

impl fmt::Display for TypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExtension::ScalarTypeExtension(v) => write!(f, "{}", v),
            TypeExtension::ObjectTypeExtension(v) => write!(f, "{}", v),
            TypeExtension::InterfaceTypeExtension(v) => write!(f, "{}", v),
            TypeExtension::UnionTypeExtension(v) => write!(f, "{}", v),
            TypeExtension::EnumTypeExtension(v) => write!(f, "{}", v),
            TypeExtension::InputObjectTypeExtension(v) => write!(f, "{}", v),
        }
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ScalarTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: OptDirectiveVec,
}

impl<'a> From<&'a ScalarTypeExtension> for AST<'a> {
    fn from(s: &'a ScalarTypeExtension) -> Self {
        AST::ScalarTypeExtension(s)
    }
}

impl fmt::Display for ScalarTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extend scalar{}{}", self.name, self.directives)
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub interfaces: Option<Vec<NamedType>>,
    pub directives: OptDirectiveVec,
    pub fields: OptFieldDefinitionVec,
}

impl<'a> From<&'a ObjectTypeExtension> for AST<'a> {
    fn from(s: &'a ObjectTypeExtension) -> Self {
        AST::ObjectTypeExtension(s)
    }
}

impl fmt::Display for ObjectTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let interface_data = if let Some(itf) = &self.interfaces {
            itf.iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join(" & ")
        } else {
            "".to_string()
        };
        write!(
            f,
            "extend type {}{}{}{}",
            self.name, interface_data, self.directives, self.fields
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InterfaceTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: OptDirectiveVec,
    pub fields: OptFieldDefinitionVec,
}

impl<'a> From<&'a InterfaceTypeExtension> for AST<'a> {
    fn from(s: &'a InterfaceTypeExtension) -> Self {
        AST::InterfaceTypeExtension(s)
    }
}

impl fmt::Display for InterfaceTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend interface {}{}{}",
            self.name, self.directives, self.fields
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnionTypeExtension {
    pub loc: Option<Location>,
    pub name: Name,
    pub directives: OptDirectiveVec,
    pub types: Option<Vec<NamedType>>,
}

impl<'a> From<&'a UnionTypeExtension> for AST<'a> {
    fn from(s: &'a UnionTypeExtension) -> Self {
        AST::UnionTypeExtension(s)
    }
}

impl fmt::Display for UnionTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let union_types = if let Some(types) = &self.types {
            let mut val = types
                .iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>()
                .join(" | ");
            val.insert_str(0, " = ");
            val
        } else {
            "".to_string()
        };
        write!(
            f,
            "extend union {}{}{}",
            self.name, self.directives, union_types
        )
    }
}

#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumTypeExtension {
    /// The location of the enum extension in the source text
    pub loc: Option<Location>,
    /// The name of the "target" enum
    pub name: Name,
    /// A list of directives assigned to the enum extension
    pub directives: OptDirectiveVec,
    /// A list of possible values to extend the enum by.
    pub values: OptEnumValueDefinitionVec,
}

impl<'a> From<&'a EnumTypeExtension> for AST<'a> {
    fn from(s: &'a EnumTypeExtension) -> Self {
        AST::EnumTypeExtension(s)
    }
}

impl fmt::Display for EnumTypeExtension {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "extend enum {}{}{}",
            self.name, self.directives, self.values
        )
    }
}

/// An input object type extension can be used to add fields to an existing input object
#[serde(tag = "kind")]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputObjectTypeExtension {
    /// The location of the input object extension in the source text
    pub loc: Option<Location>,
    /// The name of the "target" input object
    pub name: Name,
    /// A list of directives assigned to the input object extension
    pub directives: OptDirectiveVec,
    /// A list of fields that the input object extension has
    pub fields: OptInputValueDefinitionVec,
}

impl<'a> From<&'a InputObjectTypeExtension> for AST<'a> {
    fn from(s: &'a InputObjectTypeExtension) -> Self {
        AST::InputObjectTypeExtension(s)
    }
}

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
