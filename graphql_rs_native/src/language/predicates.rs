use super::ast::*;

pub fn is_definition_node(node: &'_ AST<'_, '_>) -> bool {
    is_executable_definition_node(node)
        || is_type_system_definition_node(node)
        || is_type_system_extension_node(node)
}
pub fn is_executable_definition_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::OperationDefinition(_) | AST::FragmentDefinition(_) => true,
        _ => false,
    }
}
pub fn is_selection_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::Field(_) | AST::FragmentSpread(_) | AST::InlineFragment(_) => true,
        _ => false,
    }
}
pub fn is_value_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::Variable(_)
        | AST::IntValue(_)
        | AST::FloatValue(_)
        | AST::StringValue(_)
        | AST::BooleanValue(_)
        | AST::NullValue(_)
        | AST::EnumValue(_)
        | AST::ListValue(_)
        | AST::ObjectValue(_) => true,
        _ => false,
    }
}
pub fn is_type_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::NamedType(_) | AST::NonNullType(_) | AST::ListType(_) => true,
        _ => false,
    }
}
pub fn is_type_system_definition_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::DirectiveDefinition(_) | AST::SchemaDefinition(_) => true,
        _ => is_type_definition_node(node),
    }
}
pub fn is_type_definition_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::ScalarTypeDefinition(_)
        | AST::ObjectTypeDefinition(_)
        | AST::InterfaceTypeDefinition(_)
        | AST::UnionTypeDefinition(_)
        | AST::EnumTypeDefinition(_)
        | AST::InputObjectTypeDefinition(_) => true,
        _ => false,
    }
}
pub fn is_type_system_extension_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::SchemaExtension(_) => true,
        _ => is_type_extension_node(node),
    }
}
pub fn is_type_extension_node(node: &'_ AST<'_, '_>) -> bool {
    match node {
        AST::ScalarTypeExtension(_)
        | AST::ObjectTypeExtension(_)
        | AST::InterfaceTypeExtension(_)
        | AST::UnionTypeExtension(_)
        | AST::EnumTypeExtension(_)
        | AST::InputObjectTypeExtension(_) => true,
        _ => false,
    }
}
