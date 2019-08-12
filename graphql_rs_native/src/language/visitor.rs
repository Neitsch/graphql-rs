use super::ast::*;

pub trait Visitor<'a> {
    fn visit_name(&mut self, _: &'a Name) {}
    fn visit_document(&mut self, _: &'a Document) {}
    fn visit_operation_definition(&mut self, _: &'a OperationDefinition) {}
    fn visit_variable_definition(&mut self, _: &'a VariableDefinition) {}
    fn visit_variable(&mut self, _: &'a Variable) {}
    fn visit_selection_set(&mut self, _: &'a SelectionSet) {}
    fn visit_field(&mut self, _: &'a Field) {}
    fn visit_argument(&mut self, _: &'a Argument) {}
    fn visit_fragment_spread(&mut self, _: &'a FragmentSpread) {}
    fn visit_inline_fragment(&mut self, _: &'a InlineFragment) {}
    fn visit_fragment_definition(&mut self, _: &'a FragmentDefinition) {}
    fn visit_int_value(&mut self, _: &'a IntValue) {}
    fn visit_float_value(&mut self, _: &'a FloatValue) {}
    fn visit_string_value(&mut self, _: &'a StringValue) {}
    fn visit_boolean_value(&mut self, _: &'a BooleanValue) {}
    fn visit_null_value(&mut self, _: &'a NullValue) {}
    fn visit_enum_value(&mut self, _: &'a EnumValue) {}
    fn visit_list_value(&mut self, _: &'a ListValue) {}
    fn visit_object_value(&mut self, _: &'a ObjectValue) {}
    fn visit_object_field(&mut self, _: &'a ObjectField) {}
    fn visit_directive(&mut self, _: &'a Directive) {}
    fn visit_named_type(&mut self, _: &'a NamedType) {}
    fn visit_list_type(&mut self, _: &'a ListType) {}
    fn visit_non_null_type(&mut self, _: &'a NonNullType) {}
    fn visit_schema_definition(&mut self, _: &'a SchemaDefinition) {}
    fn visit_operation_type_definition(&mut self, _: &'a OperationTypeDefinition) {}
    fn visit_scalar_type_definition(&mut self, _: &'a ScalarTypeDefinition) {}
    fn visit_object_type_definition(&mut self, _: &'a ObjectTypeDefinition) {}
    fn visit_field_definition(&mut self, _: &'a FieldDefinition) {}
    fn visit_input_value_definition(&mut self, _: &'a InputValueDefinition) {}
    fn visit_interface_type_definition(&mut self, _: &'a InterfaceTypeDefinition) {}
    fn visit_union_type_definition(&mut self, _: &'a UnionTypeDefinition) {}
    fn visit_enum_type_definition(&mut self, _: &'a EnumTypeDefinition) {}
    fn visit_enum_value_definition(&mut self, _: &'a EnumValueDefinition) {}
    fn visit_input_object_type_definition(&mut self, _: &'a InputObjectTypeDefinition) {}
    fn visit_directive_definition(&mut self, _: &'a DirectiveDefinition) {}
    fn visit_schema_extension(&mut self, _: &'a SchemaExtension) {}
    fn visit_scalar_type_extension(&mut self, _: &'a ScalarTypeExtension) {}
    fn visit_object_type_extension(&mut self, _: &'a ObjectTypeExtension) {}
    fn visit_interface_type_extension(&mut self, _: &'a InterfaceTypeExtension) {}
    fn visit_union_type_extension(&mut self, _: &'a UnionTypeExtension) {}
    fn visit_enum_type_extension(&mut self, _: &'a EnumTypeExtension) {}
    fn visit_input_object_type_extension(&mut self, _: &'a InputObjectTypeExtension) {}
}

#[allow(dead_code)]
fn visit<'a>(visitor: &mut dyn Visitor<'a>, node: AST<'a>) {
    match node {
        AST::Name(inner_node) => visitor.visit_name(inner_node),
        AST::Document(inner_node) => visitor.visit_document(inner_node),
        AST::OperationDefinition(inner_node) => visitor.visit_operation_definition(inner_node),
        AST::VariableDefinition(inner_node) => visitor.visit_variable_definition(inner_node),
        AST::Variable(inner_node) => visitor.visit_variable(inner_node),
        AST::SelectionSet(inner_node) => visitor.visit_selection_set(inner_node),
        AST::Field(inner_node) => visitor.visit_field(inner_node),
        AST::Argument(inner_node) => visitor.visit_argument(inner_node),
        AST::FragmentSpread(inner_node) => visitor.visit_fragment_spread(inner_node),
        AST::InlineFragment(inner_node) => visitor.visit_inline_fragment(inner_node),
        AST::FragmentDefinition(inner_node) => visitor.visit_fragment_definition(inner_node),
        AST::IntValue(inner_node) => visitor.visit_int_value(inner_node),
        AST::FloatValue(inner_node) => visitor.visit_float_value(inner_node),
        AST::StringValue(inner_node) => visitor.visit_string_value(inner_node),
        AST::BooleanValue(inner_node) => visitor.visit_boolean_value(inner_node),
        AST::NullValue(inner_node) => visitor.visit_null_value(inner_node),
        AST::EnumValue(inner_node) => visitor.visit_enum_value(inner_node),
        AST::ListValue(inner_node) => visitor.visit_list_value(inner_node),
        AST::ObjectValue(inner_node) => visitor.visit_object_value(inner_node),
        AST::ObjectField(inner_node) => visitor.visit_object_field(inner_node),
        AST::Directive(inner_node) => visitor.visit_directive(inner_node),
        AST::NamedType(inner_node) => visitor.visit_named_type(inner_node),
        AST::ListType(inner_node) => visitor.visit_list_type(inner_node),
        AST::NonNullType(inner_node) => visitor.visit_non_null_type(inner_node),
        AST::SchemaDefinition(inner_node) => visitor.visit_schema_definition(inner_node),
        AST::OperationTypeDefinition(inner_node) => {
            visitor.visit_operation_type_definition(inner_node)
        }
        AST::ScalarTypeDefinition(inner_node) => visitor.visit_scalar_type_definition(inner_node),
        AST::ObjectTypeDefinition(inner_node) => visitor.visit_object_type_definition(inner_node),
        AST::FieldDefinition(inner_node) => visitor.visit_field_definition(inner_node),
        AST::InputValueDefinition(inner_node) => visitor.visit_input_value_definition(inner_node),
        AST::InterfaceTypeDefinition(inner_node) => {
            visitor.visit_interface_type_definition(inner_node)
        }
        AST::UnionTypeDefinition(inner_node) => visitor.visit_union_type_definition(inner_node),
        AST::EnumTypeDefinition(inner_node) => visitor.visit_enum_type_definition(inner_node),
        AST::EnumValueDefinition(inner_node) => visitor.visit_enum_value_definition(inner_node),
        AST::InputObjectTypeDefinition(inner_node) => {
            visitor.visit_input_object_type_definition(inner_node)
        }
        AST::DirectiveDefinition(inner_node) => visitor.visit_directive_definition(inner_node),
        AST::SchemaExtension(inner_node) => visitor.visit_schema_extension(inner_node),
        AST::ScalarTypeExtension(inner_node) => visitor.visit_scalar_type_extension(inner_node),
        AST::ObjectTypeExtension(inner_node) => visitor.visit_object_type_extension(inner_node),
        AST::InterfaceTypeExtension(inner_node) => {
            visitor.visit_interface_type_extension(inner_node)
        }
        AST::UnionTypeExtension(inner_node) => visitor.visit_union_type_extension(inner_node),
        AST::EnumTypeExtension(inner_node) => visitor.visit_enum_type_extension(inner_node),
        AST::InputObjectTypeExtension(inner_node) => {
            visitor.visit_input_object_type_extension(inner_node)
        }
    }
    step_into(visitor, node);
}

fn step_into<'a>(visitor: &mut dyn Visitor<'a>, node: AST<'a>) {
    match node {
        AST::Name(inner_node) => {}
        AST::Document(inner_node) => inner_node
            .definitions
            .iter()
            .map(|inner_node| visit(visitor, inner_node.into()))
            .collect(),
        AST::OperationDefinition(inner_node) => {
            std::iter::empty()
            .chain(inner_node.directives.map(|x| x.iter()).iter().flatten().map(|x| x.into()))
            .map(|x| visit(visitor, x))
            .collect()
        },
        AST::VariableDefinition(inner_node) => visitor.visit_variable_definition(inner_node) => {

        },
        AST::Variable(inner_node) => visitor.visit_variable(inner_node),
        AST::SelectionSet(inner_node) => visitor.visit_selection_set(inner_node),
        AST::Field(inner_node) => visitor.visit_field(inner_node),
        AST::Argument(inner_node) => visitor.visit_argument(inner_node),
        AST::FragmentSpread(inner_node) => visitor.visit_fragment_spread(inner_node),
        AST::InlineFragment(inner_node) => visitor.visit_inline_fragment(inner_node),
        AST::FragmentDefinition(inner_node) => visitor.visit_fragment_definition(inner_node),
        AST::IntValue(inner_node) => visitor.visit_int_value(inner_node),
        AST::FloatValue(inner_node) => visitor.visit_float_value(inner_node),
        AST::StringValue(inner_node) => visitor.visit_string_value(inner_node),
        AST::BooleanValue(inner_node) => visitor.visit_boolean_value(inner_node),
        AST::NullValue(inner_node) => visitor.visit_null_value(inner_node),
        AST::EnumValue(inner_node) => visitor.visit_enum_value(inner_node),
        AST::ListValue(inner_node) => visitor.visit_list_value(inner_node),
        AST::ObjectValue(inner_node) => visitor.visit_object_value(inner_node),
        AST::ObjectField(inner_node) => visitor.visit_object_field(inner_node),
        AST::Directive(inner_node) => visitor.visit_directive(inner_node),
        AST::NamedType(inner_node) => visitor.visit_named_type(inner_node),
        AST::ListType(inner_node) => visitor.visit_list_type(inner_node),
        AST::NonNullType(inner_node) => visitor.visit_non_null_type(inner_node),
        AST::SchemaDefinition(inner_node) => visitor.visit_schema_definition(inner_node),
        AST::OperationTypeDefinition(inner_node) => {
            visitor.visit_operation_type_definition(inner_node)
        }
        AST::ScalarTypeDefinition(inner_node) => visitor.visit_scalar_type_definition(inner_node),
        AST::ObjectTypeDefinition(inner_node) => visitor.visit_object_type_definition(inner_node),
        AST::FieldDefinition(inner_node) => visitor.visit_field_definition(inner_node),
        AST::InputValueDefinition(inner_node) => visitor.visit_input_value_definition(inner_node),
        AST::InterfaceTypeDefinition(inner_node) => {
            visitor.visit_interface_type_definition(inner_node)
        }
        AST::UnionTypeDefinition(inner_node) => visitor.visit_union_type_definition(inner_node),
        AST::EnumTypeDefinition(inner_node) => visitor.visit_enum_type_definition(inner_node),
        AST::EnumValueDefinition(inner_node) => visitor.visit_enum_value_definition(inner_node),
        AST::InputObjectTypeDefinition(inner_node) => {
            visitor.visit_input_object_type_definition(inner_node)
        }
        AST::DirectiveDefinition(inner_node) => visitor.visit_directive_definition(inner_node),
        AST::SchemaExtension(inner_node) => visitor.visit_schema_extension(inner_node),
        AST::ScalarTypeExtension(inner_node) => visitor.visit_scalar_type_extension(inner_node),
        AST::ObjectTypeExtension(inner_node) => visitor.visit_object_type_extension(inner_node),
        AST::InterfaceTypeExtension(inner_node) => {
            visitor.visit_interface_type_extension(inner_node)
        }
        AST::UnionTypeExtension(inner_node) => visitor.visit_union_type_extension(inner_node),
        AST::EnumTypeExtension(inner_node) => visitor.visit_enum_type_extension(inner_node),
        AST::InputObjectTypeExtension(inner_node) => {
            visitor.visit_input_object_type_extension(inner_node)
        }
    }
}
