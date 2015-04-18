#' Collection of tools to simplify routine aquatic chemistry calculations.
#' 
#' This package includes a basic toolbox for units management with certain
#' types of \link{quantities} (\link{volume}, \link{concentration}, etc.) and full
#' support for \link{metric} scaling.
#' 
#' @name chemtools
#' @docType package
#' @title chemtools package
#' @author Sebastian Kopf
#' @seealso \link{quantity}, \link{metric}
NULL

#' @include utils.R
#' @include constants.R
#' @include classes.R
#' @include units.R
NULL

# constants
kPa_per_atm = 101.325         # conversion from kPa to atmosphere
psi_per_atm = 14.696          # conversion from psi to atmosphere
R_ml_atm_per_K_mol = 82.05746 # ideal gas constant

#' Ideal gas object
#' 
#' @exportClass IdealGas
IdealGas <- setRefClass(
  "IdealGas",
  fields = list(
    formula = "character",
    mol_weight = "numeric",   # in g/mol
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