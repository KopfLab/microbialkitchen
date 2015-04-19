#' @include classes.R

#' @export
is.component <- function(x) inherits(x, "Component")

# Add components from an aquasys to another.
add_component <- function (p1, p2) {
  if (!is.aquasys(p1) || !is.aquasys(p2))
    stop ("This function can only work with aquasys objects") # should not happen
  
  stop("not implemented")
}

#Note: if want to be able to call the constructor directly (as Component, instead of Component$new())
#' Component
#' @name Component
#' @export
NULL

#' @description This is the component class
#' @rdname Component
#' @exportClass Component
Component <- setRefClass(
  "Component",
  fields = list(
    formula = "character",
    mw = "MolecularWeight"
  ))

#' Ideal gas object
#' 
#' @exportClass IdealGas
IdealGas <- setRefClass(
  "IdealGas", contains = "Component",
  fields = list(
    henry = "numeric"         # in atm kg / mol
  ),
  methods = list(
    initialize = function(formula, mol_weight = NULL, 
                          henry_atm_kg_per_mol = NULL,
                          henry_kPa_kg_per_mol = NULL) {
      callSuper(formula, mol_weight, henry = henry_atm_kg_mol)
    }
  )
)