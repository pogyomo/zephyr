use thiserror::Error;
use zephyr_ast::Program;

#[derive(Debug, Error)]
pub enum TypeCheckError {}

/// Check types of given program and add correct type to objects.
pub fn type_check(_prog: Program) -> Result<Program, TypeCheckError> {
    todo!()
}
