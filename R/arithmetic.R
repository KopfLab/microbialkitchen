# make sure classes are loaded before everything else
#' @include classes.R
NULL

#' Arithmetic
#' 
#' These arithmetic operators for quantities in mediachemtools keep track of units in standard operations (i.e. they ALL take the SI prefix into consideration). Also note that all operations that result in a new quantity object automatically scale the new value using \code{\link{best_metric}}.
#' 
#' @name arithmetic
NULL


# Comparisons =======================

#' @usage qty == qty, qty != qty, qty < qty, qty <= qty, qty > qty, qty >= qty
#' @details
#' \code{qty ==, !=, <, <=, >, >= qty} allows the comparison of quantities that are the same type (e.g. all mass).
#' @examples 
#' qty(1, "g") == qty(1000, "mg") # TRUE
#' qty(2, "mg") < qty(5, "ng")  # FALSE
#' @name arithmetic 
NULL

setMethod("<", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data < e2@.Data)
})

setMethod("<=", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data <= e2@.Data)
})

setMethod(">", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data > e2@.Data)
})

setMethod(">=", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data >= e2@.Data)
})

setMethod("==", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data == e2@.Data)
})

setMethod("!=", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("comparison", e1, e2)
  return (scale_metric(e1, get_prefix(e2))@.Data != e2@.Data)
})

# the following are not allowed b/c of ambiguity (no unit for second number)
# update: these are used so frequently in other functions (e.g. ggplots), it is difficult to restrict them

# setMethod("<", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("<", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("<=", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("<=", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod(">", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod(">", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod(">=", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod(">=", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("==", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("==", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("!=", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
#   operation_error("comparison", e1, e2))
# 
# setMethod("!=", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
#   operation_error("comparison", e1, e2))

# Addition  ========================

#' @usage qty + qty, qty - qty
#' @details
#' \code{qty +- qty} allows the addition/subtraction of quantities that are the same type (e.g. all mass). Note that this operation also scales the new value using \code{\link{best_metric}}.
#' @examples 
#' qty(1000, "mg") + qty(999, "g") # 1 kg
#' @name arithmetic 
NULL


setMethod("+", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  class_check ("addition", e1, e2)
  e1 <- scale_metric(e1, get_prefix(e2))
  e1@.Data <- e1@.Data + e2@.Data
  return(best_metric(e1))
})

setMethod("-", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2)  {
  class_check ("subtraction", e1, e2)
  e1 <- scale_metric(e1, get_prefix(e2))
  e1@.Data <- e1@.Data - e2@.Data
  return(best_metric(e1))
})

# the following are not allowed b/c of ambiguity (no unit for second number)
setMethod("+", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
  operation_error("addition", e1, e2))

setMethod("+", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
  operation_error("addition", e1, e2))

setMethod("-", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) 
  operation_error("subtraction", e1, e2))

setMethod("-", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) 
  operation_error("subtraction", e1, e2))

# Division =========================

#' @usage qty / qty
#' @details
#' \code{qty / qty} divide quantities of the same type. Returns plain numeric (i.e. the units are divided out).
#' @examples 
#' qty(5, "mg") / qty(1, "g") # 0.005
#' @name arithmetic 
NULL

setMethod("/", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  if (class(e1) == class(e2))
    return (scale_metric(e1, get_prefix(e2))@.Data / e2@.Data)
  else
    operation_error ("division", e1, e2)
})

#' @usage qty / number
#' @details
#' \code{qty / number} divide quantity by a plain number. Returns the quantity automatically rescaled to the best metric.
#' @examples 
#' qty(5, "mg") / 1e6 # 5 ng
#' @name arithmetic 
NULL

setMethod("/", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) {
  e1@.Data <- e1@.Data / e2
  return (best_metric(e1))
})

# don't allow number / quantity
setMethod("/", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  operation_error ("division", e1, e2)
})

# Multiplication =========================

#' @usage qty * number
#' @details
#' \code{qty * number} multiply a quantity by a plain number (either way around is valid). Returns the quantity automatically rescaled to the best metric.
#' @examples 
#' qty(5, "mg") * 1e6 # 5 kg
#' @name arithmetic 
NULL

# don't allow default quantity * quantity
setMethod("*", signature(e1 = "MediaChemToolsQuantity", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  operation_error ("multiplication", e1, e2)
})

# don't allow default quantity * quantity
setMethod("*", signature(e1 = "MediaChemToolsQuantity", e2 = "numeric"), function(e1, e2) {
  e1@.Data <- e1@.Data * e2
  return (best_metric(e1))
})

setMethod("*", signature(e1 = "numeric", e2 = "MediaChemToolsQuantity"), function(e1, e2) {
  return (e2 * e1)
})

# Special operations ==============

#' @usage amount / volume = molarity, amount / molarity = volume, molarity * volume = amount
#' @details \code{amount / volume} divide an amount by a volume to create a molarity (concentration).
#' @details \code{amount / molarity} divide an amount by a molarity to create a volume. 
#' @details \code{molarity * volume} multiply a molarity by a volume (or the other way around) to create an amount.  
#' @examples 
#' qty(5, "nmol") / qty(50, "mL") # 100 nM
#' qty(5, "nmol") / qty(100, "nM") # 50 mL
#' qty(100, "nM") * qty(50, "mL") # 5 nmol
#' @name arithmetic 
NULL

# amount / volume = molarity
setMethod("/", signature(e1 = "MediaChemToolsAmount", e2 = "MediaChemToolsVolume"), function(e1, e2) {
  return (molarity( e1@.Data / base_metric(e2)@.Data, paste0(e1@unit, "/L") ))
})

# amount / molarity = volume
setMethod("/", signature(e1 = "MediaChemToolsAmount", e2 = "MediaChemToolsMolarity"), function(e1, e2) {
  return (volume( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "L" ))
})

# volume * molarity = amount
setMethod("*", signature(e1 = "MediaChemToolsVolume", e2 = "MediaChemToolsMolarity"), function(e1, e2) {
  return (amount( base_metric(e1)@.Data * base_metric(e2)@.Data, "mol"))
})
setMethod("*", signature(e1 = "MediaChemToolsMolarity", e2 = "MediaChemToolsVolume"), function(e1, e2) e2 * e1)


#' @usage mass / volume = density, mass / density = volume, density * volume = mass
#' @details \code{mass / volume} divide a mass by a volume to create a density (concentration).
#' @details \code{mass / density} divide a mass by a density to create a volume. 
#' @details \code{density * volume} multiply a density by a volume (or the other way around) to create a mass.  
#' @examples 
#' qty(5, "ng") / qty(50, "mL") # 100 ng/L
#' qty(5, "ng") / qty(100, "ng/L") # 50 mL
#' qty(100, "ng/L") * qty(50, "mL") # 5 ng
#' @name arithmetic 
NULL

# amount / volume = molarity
setMethod("/", signature(e1 = "MediaChemToolsMass", e2 = "MediaChemToolsVolume"), function(e1, e2) {
  return (density( e1@.Data / base_metric(e2)@.Data, paste0(e1@unit, "/L") ))
})

# amount / molarity = volume
setMethod("/", signature(e1 = "MediaChemToolsMass", e2 = "MediaChemToolsDensity"), function(e1, e2) {
  return (volume( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "L" ))
})

# volume * molarity = amount
setMethod("*", signature(e1 = "MediaChemToolsVolume", e2 = "MediaChemToolsDensity"), function(e1, e2) {
  return (mass( base_metric(e1)@.Data * base_metric(e2)@.Data, "g"))
})
setMethod("*", signature(e1 = "MediaChemToolsDensity", e2 = "MediaChemToolsVolume"), function(e1, e2) e2 * e1)


#' @usage mass / molecular mass = amount, mass / amount = molecular mass, amount * molecular mass = mass
#' @details \code{mass / MW} divide a mass by a molecular mass to create an amount (mols).
#' @details \code{mass / amount} divice a mass by an amount (mols) to create a molecular mass.
#' @details \code{amount * MW} multiply an amount (mols) by a molecular mass to create a mass.
#' @examples 
#' qty(10, "g") / qty (50, "g/mol") # 200 nmol
#' qty(10, "g") / qty(200, "mmol") # 50 g/mol
#' qty(200, "mmol") * qty (50, "g/mol") # 10 g
#' @name arithmetic 
NULL

# mass / MW = amount
setMethod("/", signature(e1 = "MediaChemToolsMass", e2 = "MediaChemToolsMolecularMass"), function(e1, e2) {
  return (amount( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "mol" ))
})

# mass / amount = MW
setMethod("/", signature(e1 = "MediaChemToolsMass", e2 = "MediaChemToolsAmount"), function(e1, e2) {
  return (molecular_mass( base_metric(e1)@.Data / base_metric(e2)@.Data, "g/mol") )
})

# amount * MW = mass
setMethod("*", signature(e1 = "MediaChemToolsAmount", e2 = "MediaChemToolsMolecularMass"), function(e1, e2) {
  return (mass( base_metric(e1)@.Data * base_metric(e2)@.Data, "g"))
})
setMethod("*", signature(e1 = "MediaChemToolsMolecularMass", e2 = "MediaChemToolsAmount"), function(e1, e2) e2 * e1)

#' @usage molarity / pressure = solubility, solubility * pressure = molarity, molarity / solubility = pressure
#' @details \code{molarity / pressure} divide a concentration by pressure to create a solubility (M/bar).
#' @details \code{solubility * pressure} multiply a solubility (M/bar) by pressure (bar) to create molarity (M)
#' @details \code{molarity / solubility} divide a molarity (M) by a solubility (M/bar) to get partial pressure (bar)
#' @examples 
#' qty(10, "mM") / qty(200, "mbar") # 50 mM/bar
#' qty(10, "mM") / qty(50, "mM/bar") # 200 mbar
#' qty(50, "mM/bar") * qty (200, "mbar") # 10 mM
#' @name arithmetic 
NULL

# molarity / pressure = solubility
setMethod("/", signature(e1 = "MediaChemToolsMolarity", e2 = "MediaChemToolsPressure"), function(e1, e2) {
  return (solubility( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "M/bar" ))
})

# molarity / solubility = pressure
setMethod("/", signature(e1 = "MediaChemToolsMolarity", e2 = "MediaChemToolsSolubility"), function(e1, e2) {
  return (pressure( base_metric(e1)@.Data / base_metric(e2)@.Data, "bar") )
})

# solubility * pressure = molarity
setMethod("*", signature(e1 = "MediaChemToolsSolubility", e2 = "MediaChemToolsPressure"), function(e1, e2) {
  return (molarity( base_metric(e1)@.Data * base_metric(e2)@.Data, "M"))
})
setMethod("*", signature(e1 = "MediaChemToolsPressure", e2 = "MediaChemToolsSolubility"), function(e1, e2) e2 * e1)



