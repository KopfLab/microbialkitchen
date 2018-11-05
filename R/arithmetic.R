# make sure classes are loaded before everything else
#' @include classes.R
NULL

#' Arithmetic
#' 
#' These arithmetic operators for quantities in chemtools keep track of units in standard operations (i.e. they ALL take the SI prefix into consideration). Also note that all operations that result in a new quantity object automatically scale the new value using \code{\link{cht_best_metric}}.
#' 
#' @name arithmetic
NULL



# helper functions that informs about operation error
operation_error <- function(operation, e1, e2) {
  stop(sprintf("%s is not implemented for these quantities (trying to use '%s' and '%s'). ", operation, class(e1), class(e2)), call. = FALSE)
}
class_check <- function(operation, e1, e2) {
  if (class(e1) != class(e2)) operation_error(operation, e1, e2)
}

# Comparisons =======================

#' @usage qty == qty, qty != qty, qty < qty, qty <= qty, qty > qty, qty >= qty
#' @details
#' \code{qty ==, !=, <, <=, >, >= qty} allows the comparison of quantities that are the same type (e.g. all mass).
#' @examples 
#' cht_qty(1, "g") == cht_qty(1000, "mg") # TRUE
#' cht_qty(2, "mg") < cht_qty(5, "ng")  # FALSE
#' @name arithmetic 
NULL

setMethod("<", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (cht_scale_metric(e1, get_prefix(e2))@.Data < e2@.Data)
})

setMethod("<=", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (cht_scale_metric(e1, get_prefix(e2))@.Data <= e2@.Data)
})

setMethod(">", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (cht_scale_metric(e1, get_prefix(e2))@.Data > e2@.Data)
})

setMethod(">=", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (cht_scale_metric(e1, get_prefix(e2))@.Data >= e2@.Data)
})

setMethod("==", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (cht_scale_metric(e1, get_prefix(e2))@.Data == e2@.Data)
})

setMethod("!=", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (cht_scale_metric(e1, get_prefix(e2))@.Data != e2@.Data)
})

# Addition  ========================

#' @usage qty + qty, qty - qty
#' @details
#' \code{qty +- qty} allows the addition/subtraction of quantities that are the same type (e.g. all mass). Note that this operation also scales the new value using \code{\link{cht_best_metric}}.
#' @examples 
#' cht_qty(1000, "mg") + cht_qty(999, "g") # 1 kg
#' @name arithmetic 
NULL

setMethod("+", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  class_check ("addition", e1, e2)
  return(cht_best_metric(cht_scale_metric(e1, get_prefix(e2)) + e2@.Data))
})

setMethod("-", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2)  e1 + -1* e2)

# Division =========================

#' @usage qty / qty
#' @details
#' \code{qty / qty} divide quantities of the same type. Returns plain numeric (i.e. the units are divided out).
#' @examples 
#' cht_qty(5, "mg") / cht_qty(1, "g") # 0.005
#' @name arithmetic 
NULL

setMethod("/", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  if (class(e1) == class(e2))
    return (cht_scale_metric(e1, get_prefix(e2))@.Data / e2@.Data)
  else
    operation_error ("division", e1, e2)
})

# don't allow number / quantity
setMethod("/", signature(e1 = "numeric", e2 = "Quantity"), function(e1, e2) {
  operation_error ("division", e1, e2)
})

# Multiplication =========================

# don't allow default quantity * quantity
setMethod("*", signature(e1 = "Quantity", e2 = "Quantity"), function(e1, e2) {
  operation_error ("multiplication", e1, e2)
})

# Special operations ==============

#' @usage amount / volume = molarity, amount / molarity = volume, molarity * volume = amount
#' @details \code{amount / volume} divide an amount by a volume to create a molarity (concentration).
#' @details \code{amount / molarity} divide an amount by a molarity to create a volume. 
#' @details \code{molarity * volume} multiply a molarity by a volume (or the other way around) to create an amount.  
#' @examples 
#' cht_qty(5, "nmol") / cht_qty(50, "mL") # 100 nM
#' cht_qty(5, "nmol") / cht_qty(100, "nM") # 50 mL
#' cht_qty(100, "nM") * cht_qty(50, "mL") # 5 nmol
#' @name arithmetic 
NULL

# amount / volume = molarity
setMethod("/", signature(e1 = "Amount", e2 = "Volume"), function(e1, e2) {
  return (cht_concentration( e1@.Data / cht_base_metric(e2)@.Data, paste0(e1@unit, "/L") ))
})

# amount / molarity = volume
setMethod("/", signature(e1 = "Amount", e2 = "Molarity"), function(e1, e2) {
  return (cht_volume( cht_scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "L" ))
})

# volume * molarity = amount
setMethod("*", signature(e1 = "Volume", e2 = "Molarity"), function(e1, e2) {
  return (cht_amount( cht_base_metric(e1)@.Data * cht_base_metric(e2)@.Data, "mol", scale = TRUE))
})
setMethod("*", signature(e1 = "Molarity", e2 = "Volume"), function(e1, e2) e2 * e1)

#' @usage mass / MW = amount, mass / amount = MW, amount * MW = mass
#' @details \code{mass / MW} divide a mass by a molecular weight to create an amount (mols).
#' @details \code{mass / amount} divice a mass by an amount (mols) to create a molecular weight.
#' @details \code{amount * MW} multiply an amount (mols) by a molecular weight to create a mass.
#' @examples 
#' cht_qty(10, "g") / qty (50, "g/mol") # 200 mM 
#' cht_qty(10, "g") / cht_qty(200, "mmol") # 50 g/mol
#' cht_qty(200, "mmol") * qty (50, "g/mol") # 10 g
#' @name arithmetic 
NULL

# mass / MW = amount
setMethod("/", signature(e1 = "Mass", e2 = "MolecularWeight"), function(e1, e2) {
  return (cht_amount( cht_scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "mol" ))
})

# mass / amount = MW
setMethod("/", signature(e1 = "Mass", e2 = "Amount"), function(e1, e2) {
  return (cht_molecular_weight( cht_base_metric(e1)@.Data / cht_base_metric(e2)@.Data, "g/mol", scale = FALSE) )
})

# amount * MW = mass
setMethod("*", signature(e1 = "Amount", e2 = "MolecularWeight"), function(e1, e2) {
  return (cht_mass( cht_base_metric(e1)@.Data * cht_base_metric(e2)@.Data, "g", scale = TRUE))
})
setMethod("*", signature(e1 = "MolecularWeight", e2 = "Amount"), function(e1, e2) e2 * e1)

