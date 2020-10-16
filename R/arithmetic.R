#' Arithmetic
#' 
#' These arithmetic operators for quantities in microbialkitchen keep track of units in standard operations (i.e. they ALL take the SI prefix into consideration). Also note that all operations that result in a new quantity object automatically scale the new value using \code{\link{best_metric}}.
#' 
#' @name arithmetic
NULL


# Comparisons =======================
# Note: comparisons work automatically because of vctrs implementation

#' @param qty quantity
#' @param number regular number
#' @details
#' \code{qty ==, !=, <, <=, >, >= qty} allows the comparison of quantities that are the same type (e.g. all mass).
#' @examples
#' qty(1, "g") == qty(1000, "mg", scale_to_best_metric = FALSE) # TRUE
#' qty(2, "mg") < qty(5, "ng")  # FALSE
#' @name arithmetic
NULL

# helper function to apply vector arithmetic to quantities and scale to best metric
apply_vec_arith_base_to_qty <- function(op, x, y, to = x, unit = get_qty_units(to), class = get_qty_class(to)) {
  best_metric(new_qty(vctrs::vec_arith_base(op, x, y), unit = unit, class = class))
}

#' vec_arith for microbial_kitchen_quantity objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_quantity
#' @export
#' @export vec_arith.microbial_kitchen_quantity
vec_arith.microbial_kitchen_quantity <- function(op, x, y, ...)
  UseMethod("vec_arith.microbial_kitchen_quantity", y)

#' @method vec_arith.microbial_kitchen_quantity default
#' @export
vec_arith.microbial_kitchen_quantity.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

# qty/double, qty * double ========================

#' @details
#' \code{qty / number} divide quantity by a plain number. Returns the quantity automatically rescaled to the best metric.
#' @examples
#' qty(5, "mg") / 1e6 # 5 ng
#' @name arithmetic
NULL

#' @details
#' \code{qty * number} multiply a quantity by a plain number (either way around is valid). Returns the quantity automatically rescaled to the best metric.
#' @examples
#' qty(5, "mg") * 1e6 # 5 kg
#' @name arithmetic
NULL

#' @method vec_arith.microbial_kitchen_quantity numeric
#' @export
vec_arith.microbial_kitchen_quantity.numeric <- function(op, x, y, ...) {
  # all quantities can be multiplied and divided by a regular number
  switch(
    op,
    "/" = ,
    "*" = apply_vec_arith_base_to_qty(op, x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.numeric microbial_kitchen_quantity
#' @export
vec_arith.numeric.microbial_kitchen_quantity <- function(op, x, y, ...) {
  # regular numbers can be multiplied with all quantities
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, y, x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# qty +/- qty & qty/qty  ========================

#' @details
#' \code{qty +- qty} allows the addition/subtraction of quantities that are the same type (e.g. all mass). Note that this operation also scales the new value using \code{\link{best_metric}}.
#' @examples
#' qty(1, "mg") + qty(999.999, "g") # 1 kg
#' @name arithmetic
NULL

#' @details
#' \code{qty / qty} divide quantities of the same type. Returns plain numeric (i.e. the units are divided out).
#' @examples
#' qty(5, "mg") / qty(1, "g") # 0.005
#' @name arithmetic
NULL

#' @method vec_arith.microbial_kitchen_quantity microbial_kitchen_quantity
#' @export
vec_arith.microbial_kitchen_quantity.microbial_kitchen_quantity <- function(op, x, y, ...) {
  # quantities of the same type can be added and subtracted from each other
  if (!identical(get_qty_class(x), get_qty_class(y)))
    vctrs::stop_incompatible_op(op, x, y)
  x <- base_metric(x)
  y <- base_metric(y)
  switch(
    op,
    "+" = ,
    "-" = apply_vec_arith_base_to_qty(op, x, y),
    "/" = vctrs::vec_arith_base(op, x, y),
    # rest is not allowed
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# Special operations setup ==============

#' vec_arith for microbial_kitchen_amount objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_amount
#' @export
#' @export vec_arith.microbial_kitchen_amount
vec_arith.microbial_kitchen_amount <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_amount", y)
}

#' @method vec_arith.microbial_kitchen_amount default
#' @export
vec_arith.microbial_kitchen_amount.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_mass objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_mass
#' @export
#' @export vec_arith.microbial_kitchen_mass
vec_arith.microbial_kitchen_mass <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_mass", y)
}

#' @method vec_arith.microbial_kitchen_mass default
#' @export
vec_arith.microbial_kitchen_mass.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_molecular_weight objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_molecular_weight
#' @export
#' @export vec_arith.microbial_kitchen_molecular_weight
vec_arith.microbial_kitchen_molecular_weight <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_molecular_weight", y)
}

#' @method vec_arith.microbial_kitchen_molecular_weight default
#' @export
vec_arith.microbial_kitchen_molecular_weight.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_molarity_concentration objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_molarity_concentration
#' @export
#' @export vec_arith.microbial_kitchen_molarity_concentration
vec_arith.microbial_kitchen_molarity_concentration <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_molarity_concentration", y)
}

#' @method vec_arith.microbial_kitchen_molarity_concentration default
#' @export
vec_arith.microbial_kitchen_molarity_concentration.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_mass_concentration objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_mass_concentration
#' @export
#' @export vec_arith.microbial_kitchen_mass_concentration
vec_arith.microbial_kitchen_mass_concentration <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_mass_concentration", y)
}

#' @method vec_arith.microbial_kitchen_mass_concentration default
#' @export
vec_arith.microbial_kitchen_mass_concentration.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_volume objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_volume
#' @export
#' @export vec_arith.microbial_kitchen_volume
vec_arith.microbial_kitchen_volume <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_volume", y)
}

#' @method vec_arith.microbial_kitchen_volume default
#' @export
vec_arith.microbial_kitchen_volume.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_pressure objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_pressure
#' @export
#' @export vec_arith.microbial_kitchen_pressure
vec_arith.microbial_kitchen_pressure <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_pressure", y)
}

#' @method vec_arith.microbial_kitchen_pressure default
#' @export
vec_arith.microbial_kitchen_pressure.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

#' vec_arith for microbial_kitchen_gas_solubility objects
#' @rdname vec_arith
#' @inheritParams vctrs::vec_arith
#' @method vec_arith microbial_kitchen_gas_solubility
#' @export
#' @export vec_arith.microbial_kitchen_gas_solubility
vec_arith.microbial_kitchen_gas_solubility <- function(op, x, y, ...) {
  UseMethod("vec_arith.microbial_kitchen_gas_solubility", y)
}

#' @method vec_arith.microbial_kitchen_gas_solubility default
#' @export
vec_arith.microbial_kitchen_gas_solubility.default <- function(op, x, y, ...)
  vctrs::stop_incompatible_op(op, x, y)

# molarity = amount / volume ============

#' @details \code{amount / volume} divide an amount by a volume to create a molarity (concentration).
#' @details \code{amount / molarity} divide an amount by a molarity concentration to create a volume.
#' @details \code{molarity * volume} multiply a molarity concentration by a volume (or the other way around) to create an amount.
#' @examples
#' qty(5, "nmol") / qty(50, "mL") # 100 nM
#' qty(5, "nmol") / qty(100, "nM") # 50 mL
#' qty(100, "nM") * qty(50, "mL") # 5 nmol
#' @name arithmetic
NULL

#' @method vec_arith.microbial_kitchen_amount microbial_kitchen_volume
#' @export
vec_arith.microbial_kitchen_amount.microbial_kitchen_volume <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = molarity_concentration()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_amount microbial_kitchen_molarity_concentration
#' @export
vec_arith.microbial_kitchen_amount.microbial_kitchen_molarity_concentration <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = volume()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.microbial_kitchen_volume microbial_kitchen_molarity_concentration
#' @export
vec_arith.microbial_kitchen_volume.microbial_kitchen_molarity_concentration <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = amount()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_molarity_concentration microbial_kitchen_volume
#' @export
vec_arith.microbial_kitchen_molarity_concentration.microbial_kitchen_volume <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = amount()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# density = mass / volume ==================

#' @details \code{mass / volume} divide a mass by a volume to create mass concentration (density).
#' @details \code{mass / density} divide a mass by a mass concentration (density) to create a volume.
#' @details \code{density * volume} multiply a mass concentration (density) by a volume (or the other way around) to create a mass.
#' @examples
#' qty(5, "ng") / qty(50, "mL") # 100 ng/L
#' qty(5, "ng") / qty(100, "ng/L") # 50 mL
#' qty(100, "ng/L") * qty(50, "mL") # 5 ng
#' @name arithmetic
NULL

#' @method vec_arith.microbial_kitchen_mass microbial_kitchen_volume
#' @export
vec_arith.microbial_kitchen_mass.microbial_kitchen_volume <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = mass_concentration()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_mass microbial_kitchen_mass_concentration
#' @export
vec_arith.microbial_kitchen_mass.microbial_kitchen_mass_concentration <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = volume()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.microbial_kitchen_volume microbial_kitchen_mass_concentration
#' @export
vec_arith.microbial_kitchen_volume.microbial_kitchen_mass_concentration <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = mass()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_mass_concentration microbial_kitchen_volume
#' @export
vec_arith.microbial_kitchen_mass_concentration.microbial_kitchen_volume <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = mass()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# molecular weight = mass / amount ================

#' @details \code{mass / MW} divide a mass by a molecular weight to create an amount (mols).
#' @details \code{mass / amount} divide a mass by an amount (mols) to create a molecular weight.
#' @details \code{amount * MW} multiply an amount (mols) by a molecular weight to create a mass.
#' @examples
#' qty(10, "g") / qty (50, "g/mol") # 200 mmol
#' qty(10, "g") / qty(200, "mmol") # 50 g/mol
#' qty(200, "mmol") * qty (50, "g/mol") # 10 g
#' @name arithmetic
NULL

#' @method vec_arith.microbial_kitchen_mass microbial_kitchen_molecular_weight
#' @export
vec_arith.microbial_kitchen_mass.microbial_kitchen_molecular_weight <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = amount()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_mass microbial_kitchen_amount
#' @export
vec_arith.microbial_kitchen_mass.microbial_kitchen_amount <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = molecular_weight()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.microbial_kitchen_amount microbial_kitchen_molecular_weight
#' @export
vec_arith.microbial_kitchen_amount.microbial_kitchen_molecular_weight <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = mass()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_molecular_weight microbial_kitchen_amount
#' @export
vec_arith.microbial_kitchen_molecular_weight.microbial_kitchen_amount <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = mass()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# gas solubility = molarity / pressure ===============

#' @details \code{molarity / pressure} divide a concentration by pressure to create a  gas solubility (M/bar).
#' @details \code{molarity / solubility} divide a molarity (M) by a gas solubility (M/bar) to get partial pressure (bar)
#' @details \code{solubility * pressure} multiply a gas solubility (M/bar) by pressure (bar) to create molarity (M)
#' @examples
#' qty(10, "mM") / qty(200, "mbar") # 50 mM/bar
#' qty(10, "mM") / qty(50, "mM/bar") # 200 mbar
#' qty(50, "mM/bar") * qty (200, "mbar") # 10 mM
#' @name arithmetic
NULL

#' @method vec_arith.microbial_kitchen_molarity_concentration microbial_kitchen_pressure
#' @export
vec_arith.microbial_kitchen_molarity_concentration.microbial_kitchen_pressure <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = gas_solubility()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_molarity_concentration microbial_kitchen_gas_solubility
#' @export
vec_arith.microbial_kitchen_molarity_concentration.microbial_kitchen_gas_solubility <- function(op, x, y, ...) {
  switch(
    op,
    "/" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = pressure()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.microbial_kitchen_gas_solubility microbial_kitchen_pressure
#' @export
vec_arith.microbial_kitchen_gas_solubility.microbial_kitchen_pressure <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = molarity_concentration()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.microbial_kitchen_pressure microbial_kitchen_gas_solubility
#' @export
vec_arith.microbial_kitchen_pressure.microbial_kitchen_gas_solubility <- function(op, x, y, ...) {
  switch(
    op,
    "*" = apply_vec_arith_base_to_qty(op, base_metric(x), base_metric(y), to = molarity_concentration()),
    vctrs::stop_incompatible_op(op, x, y)
  )
}



#' 
#' # molarity / pressure = solubility
#' setMethod("/", signature(e1 = "MediaChemToolsMolarity", e2 = "MediaChemToolsPressure"), function(e1, e2) {
#'   return (solubility( scale_metric(e1, get_prefix(e2))@.Data / e2@.Data, "M/bar" ))
#' })
#' 
#' # molarity / solubility = pressure
#' setMethod("/", signature(e1 = "MediaChemToolsMolarity", e2 = "MediaChemToolsSolubility"), function(e1, e2) {
#'   return (pressure( base_metric(e1)@.Data / base_metric(e2)@.Data, "bar") )
#' })
#' 
#' # solubility * pressure = molarity
#' setMethod("*", signature(e1 = "MediaChemToolsSolubility", e2 = "MediaChemToolsPressure"), function(e1, e2) {
#'   return (molarity( base_metric(e1)@.Data * base_metric(e2)@.Data, "M"))
#' })
#' setMethod("*", signature(e1 = "MediaChemToolsPressure", e2 = "MediaChemToolsSolubility"), function(e1, e2) e2 * e1)
#' 
#' 
#' 
