#' Arithmetic
#' 
#' These arithmetic operators for quantities in chemtools
#' keep track of units in standard operations.
#' 
#' @name arithmetic
#' @include classes.R
NULL

# helper functions that informs about operation error
operation_error <- function(operation, e1, e2) {
  stop(sprintf("%s is not implemented for these quantities (trying to use '%s' and '%s'). ", operation, class(e1), class(e2)))
}
class_check <- function(operation, e1, e2) {
  if (class(e1) != class(e2)) operation_error(operation, e1, e2)
}

# Comparisons =======================

setMethod("<", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data < e2@.Data)
})

setMethod("<=", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data <= e2@.Data)
})

setMethod(">", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data > e2@.Data)
})

setMethod(">=", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data >= e2@.Data)
})

# Addition  ========================
 
setMethod("+", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("addition", e1, e2)
  return(best_metric(scale_metric(e1, get_prefix(e2)) + e2@.Data))
})

setMethod("-", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2)  e1 + -1* e2)

# Division =========================

setMethod("/", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  if (class(e1) == class(e2))
    return (scale_metric(e1, get_prefix(e2))@.Data / e2@.Data)
  else
    operation_error ("division", e1, e2)
})

# amount / volume = molarity
setMethod("/", signature(e1 = "Amount", e2 = "Volume"), function(e1, e2) {
  return (concentration( e1@.Data / base_metric(e2)@.Data, paste0(e1@unit, "/L") ))
})

# amount / molarity = volume
setMethod("/", signature(e1 = "Amount", e2 = "Molarity"), function(e1, e2) {
  return (volume( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "L" ))
})

# volume * molarity = amount
setMethod("*", signature(e1 = "Volume", e2 = "Molarity"), function(e1, e2) {
  return (amount( base_metric(e1)@.Data * base_metric(e2)@.Data, "mol", scale = TRUE))
})
setMethod("*", signature(e1 = "Molarity", e2 = "Volume"), function(e1, e2) e2 * e1)

# mass / MW = amount
setMethod("/", signature(e1 = "Mass", e2 = "MolecularWeight"), function(e1, e2) {
  return (amount( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "mol" ))
})

# mass / amount = MW
setMethod("/", signature(e1 = "Mass", e2 = "Amount"), function(e1, e2) {
  return (molecular_weight( base_metric(e1)@.Data / base_metric(e2)@.Data, "g/mol", scale = FALSE) )
})

# amount * MW = mass
setMethod("*", signature(e1 = "Amount", e2 = "MolecularWeight"), function(e1, e2) {
  return (mass( base_metric(e1)@.Data * base_metric(e2)@.Data, "g", scale = TRUE))
})
setMethod("*", signature(e1 = "MolecularWeight", e2 = "Amount"), function(e1, e2) e2 * e1)

