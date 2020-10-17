# quantities ======

# qty constructor - called by other quantity functions
new_qty <- function(x = double(), unit = NA_character_, class = NULL) {
  x <- vctrs::vec_cast(x, double())
  vctrs::vec_assert(x, ptype = double())
  vctrs::vec_assert(unit, ptype = character(), size = 1)
  classes <- paste0("mk_", c(class, "quantity"))
  vctrs::new_vctr(x, unit = unit, class = classes)
}
methods::setOldClass(c("mk_quantity", "vctrs_vctr"))

#' Quantities
#'
#' The \code{qty} function makes it easy to keep track of different quantities in chemical calculations. Metric prefixes are fully supported, i.e. any unit can be combined with standard \link{metric_scaling} scaling (mL, nmol, kg, etc.). Some quantities can also be used in common \link{arithmetic} operations.
#' @details The following types of quantities are supported.
#' @name quantities
#' @aliases quantity
NULL

#' @describeIn quantities generate a quantity object
#' @param x the numeric value of the quantity, can be a single value or a vector
#' @param unit the unit of the quantity
#' @param scale_to_best_metric whether to automatically scale to the best metric prefix
#' @examples
#' qty(0.045, "mmol/L")
#' qty(200, "mbar")
#' qty(6, "psi")
#' qty(30, "C")
#' qty(100, "K")
#' qty(5, "mg/L")
#' qty(1, "mM/bar")
#' qty(257, "g/mol")
#' @export
#' @family quantity functions
qty <- function(x = double(), unit, scale_to_best_metric = TRUE) {
  if (missing(unit)) stop("no unit provided, all quantities must have a unit", call. = FALSE)
  vctrs::vec_assert(unit, ptype = character(), size = 1)
  if (!is(r <- try(molarity_concentration(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(mass_concentration(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(volume(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(amount(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(mass(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(pressure(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(gas_solubility(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(temperature(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(molecular_weight(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  stop(sprintf("Could not determine the appropriate quantity for unit '%s'", unit), call. = FALSE)
}

#' @details \emph{amount (N)}: base unit \code{mol} but also understands \code{mole}, all metric prefixes allowed
#' @name quantities
NULL
amount <- function(x = double(), unit = base_unit("amount"), scale_to_best_metric = TRUE) {
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), "mol")
  secondary_units <- paste0(names(prefix), "mole")
  if (! unit %in% c(primary_units, secondary_units))
    stop("not a known amount unit: ", unit, call. = FALSE)
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]

  # new quantity
  q <- new_qty(x, unit = unit, class = "amount")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{mass (m)}: base unit \code{g}, all metric prefixes allowed
#' @name quantities
NULL
mass <- function(x = double(), unit = base_unit("mass"), scale_to_best_metric = TRUE) {
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), "g")
  if (! unit %in% primary_units)
    stop("not a known mass unit: ", unit, call. = FALSE)

  # new quantity
  q <- new_qty(x, unit = unit, class = "mass")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{molecular weight (MW)}: base unit \code{g/mol}, all metric prefixes allowed in the numerator
#' @name quantities
NULL
molecular_weight <- function(x = double(), unit = base_unit("molecular_weight"), scale_to_best_metric = TRUE) {
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), "g/mol")
  secondary_units <- paste0(names(prefix), "Da")
  if (! unit %in% c(primary_units, secondary_units))
    stop("not a known molecular weight unit: ", unit, call. = FALSE)
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]

  # new quantity
  q <- new_qty(x, unit = unit, class = "molecular_weight")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}
# allow alias
molecular_mass <- molecular_weight

#' @details \emph{molarity concentration (C)}: base unit \code{M} but also understands \code{mol/L}, all metric prefixes allowed in the numerator
#' @name quantities
NULL
molarity_concentration <- function(x = double(), unit = base_unit("molarity_concentration"), scale_to_best_metric = TRUE) {
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), "M")
  secondary_units <- paste0(names(prefix), "mol/L")
  if (! unit %in% c(primary_units, secondary_units))
    stop("not a known molarity concentration unit: ", unit, call. = FALSE)
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]

  # new quantity
  q <- new_qty(x, unit = unit, class = "molarity_concentration")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{mass concentration (C)}: base unit \code{g/L} but also understands \code{g/l}, all metric prefixes allowed in the numerator. This is technically a density and sometimes referred to as such in the microbial kitchen documentation.
#' @name quantities
NULL
mass_concentration <- function(x = double(), unit = base_unit("mass_concentration"), scale_to_best_metric = TRUE) {
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), "g/L")
  secondary_units <- paste0(names(prefix), "g/l")
  if (! unit %in% c(primary_units, secondary_units))
    stop("not a known mass concentration unit: ", unit, call. = FALSE)
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]

  # new quantity
  q <- new_qty(x, unit = unit, class = "mass_concentration")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{volume (V)}: base unit \code{L} but also understands \code{l}, all metric prefixes allowed
#' @name quantities
NULL
volume <- function(x = double(), unit = base_unit("volume"), scale_to_best_metric = TRUE) {
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), "L")
  secondary_units <- paste0(names(prefix), "l")
  if (! unit %in% c(primary_units, secondary_units))
    stop("not a known volume unit: ", unit, call. = FALSE)
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]

  # new quantity
  q <- new_qty(x, unit = unit, class = "volume")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{pressure (P)}: base unit \code{bar} but also understands \code{Pa}, all metric prefixes allowed, the common non-metric units \code{atm}, \code{psi}, \code{Torr}, \code{mTorr}, and \code{\% SP} (\% at standard pressure = \% of 1 bar) are also supported and will be automatically converted to \code{bar}.
#' @name quantities
NULL
pressure <- function(x = double(), unit = base_unit("pressure"), scale_to_best_metric = TRUE) {
  unit_conversion <- get_pressure_unit_conversion(unit)

  # new quantity
  q <- new_qty(x * unit_conversion$conversion, unit = unit_conversion$unit, class = "pressure")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# get conversion for pressure units
get_pressure_unit_conversion <- function(unit) {
  conversion <- 1
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), unname(get_base_units()["pressure"]))
  secondary_units <- paste0(names(prefix), "Pa")
  alternative_units <- c("atm", "psi", "Torr", "mTorr", "% SP")
  if (! unit %in% c(primary_units, secondary_units, alternative_units))
    stop("not a known pressure unit: ", unit)

  # alternative units
  if (unit == "mTorr") {
    conversion <- conversion/1000
    unit <- "Torr"
  }

  if (unit == "% SP") {
    conversion <- conversion / 100
    unit <- "bar"
  } else if (unit %in% alternative_units) {
    c_factor <- get_microbialkitchen_constant(paste0("bar_per_", unit))
    conversion <- conversion * c_factor
    unit <- "bar"
  }

  # pascal
  if (unit %in% secondary_units) {
    conversion <- conversion * get_microbialkitchen_constant("bar_per_pa")
    unit <- primary_units[secondary_units == unit]
  }

  return(list(unit = unit, conversion = conversion))
}

#' @details \emph{gas solubility (S)}: base unit \code{M/bar}, all metric prefixes allowed in the numerator, the common non-metric unit \code{M/atm} is also supported and will be automatically converted to \code{M/bar}. This quantity is used for capturing Henry's law constants.
#' @name quantities
NULL
gas_solubility <- function(x = double(), unit = base_unit("gas_solubility"), scale_to_best_metric = TRUE) {
  unit_conversion <- get_gas_solubility_unit_conversion(unit)

  # new quantity
  q <- new_qty(x * unit_conversion$conversion, unit = unit_conversion$unit, class = "gas_solubility")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# get conversion for solubility units
get_gas_solubility_unit_conversion <- function(unit) {
  conversion <- 1
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), base_unit("gas_solubility"))
  secondary_units <- paste0(names(prefix), "M/atm")
  if (! unit %in% c(primary_units, secondary_units))
    stop("not a known gas solubility unit: ", unit, call. = FALSE)

  # alternative units
  if (unit %in% secondary_units) {
    c_factor <- get_microbialkitchen_constant("bar_per_atm")
    conversion <- conversion / c_factor
    unit <- primary_units[secondary_units == unit]
  }

  return(list(unit = unit, conversion = conversion))
}

#' @details \emph{temperature (T)}: base unit \code{K} but also understands \code{C} and \code{F} and converts them to Kelvin
#' @name quantities
NULL
temperature <- function(x = double(), unit = base_unit("temperature"), scale_to_best_metric = TRUE) {
  unit_conversion <- get_temperature_unit_conversion(unit)

  # new quantity
  q <- new_qty(unit_conversion$conversion_fwd(x), unit = unit_conversion$unit, class = "temperature")
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# get conversion for temperature units
get_temperature_unit_conversion <- function(unit) {
  conversion_fwd <- function(x) x
  conversion_back <- function(x) x
  prefix <- get_metric_prefixes()
  primary_units <- paste0(names(prefix), base_unit("temperature"))
  alternative_units <- c("C", "F")
  if (! unit %in% c(primary_units, alternative_units))
    stop("not a known temperature unit: ", unit, call. = FALSE)

  # alternative units
  if (unit == "C") {
    conversion_fwd <- function(x) x - get_microbialkitchen_constant("celsius_kelvin_offset")
    conversion_back <- function(x) x + get_microbialkitchen_constant("celsius_kelvin_offset")
    unit <- "K"
  } else if (unit == "F") {
    conversion_fwd <- function(x) {
      (x - get_microbialkitchen_constant("fahrenheit_celsius_offset"))/get_microbialkitchen_constant("fahrenheit_celsius_slope") -
        get_microbialkitchen_constant("celsius_kelvin_offset")
    }
    conversion_back <- function(x) {
      (x + get_microbialkitchen_constant("celsius_kelvin_offset")) * get_microbialkitchen_constant("fahrenheit_celsius_slope") +
        get_microbialkitchen_constant("fahrenheit_celsius_offset")
    }
    unit <- "K"
  }

  return(list(unit = unit, conversion_fwd = conversion_fwd, conversion_back = conversion_back))
}

# formatting ================

# get quantity class
get_qty_class <- function(q) {
  stringr::str_remove(class(q)[1], "mk_")
}

# get available abbreviations
get_abbreviations <- function() {
  get_microbialkitchen_constant("abbreviations")
}

# formatting during printout
#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full mk_quantity
#' @export
vec_ptype_full.mk_quantity <- function(x, ...) {
  qty_class <- get_qty_class(x)
  sprintf(
    "%s (%s) in '%s'",
    unname(get_abbreviations()[qty_class]),
    stringr::str_replace(qty_class, "_", " "),
    attr(x, "unit")
  )
}

#' @method format mk_quantity
#' @export
format.mk_quantity <- function(x, ...) {
  format(vctrs::vec_data(x), ...)
}

#' @importFrom pillar pillar_shaft
#' @method pillar_shaft mk_quantity
#' @export
pillar_shaft.mk_quantity <- function(x, ...) {
  pillar_shaft(vctrs::vec_data(x), ...)
}

# note: this is not yet used by rmarkdown::paged_table and hence the headers don't show up nicely, see https://github.com/rstudio/rmarkdown/issues/1487 for details
#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr mk_quantity
#' @export
vec_ptype_abbr.mk_quantity <- function(x, ...) {
  sprintf(
    "%s[%s]",
    unname(get_abbreviations()[get_qty_class(x)]),
    attr(x, "unit")
  )
}

# type checks =================

#' Check for quantity objects
#'
#' These functions check whether something is a specific type of quantity.
#'
#' @name check_quantities
#' @family quantity functions
NULL

#' @describeIn check_quantities check whether something is any quantity
#' @param q a quantity object
#' @export
is_qty <- function(q) { is(q, "mk_quantity") }

#' @describeIn check_quantities check whether something is an amount quantity
#' @export
is_amount <- function(q) { is(q, "mk_amount") }

#' @describeIn check_quantities check whether something is an amount quantity
#' @export
is_mass <- function(q) { is(q, "mk_mass") }

#' @describeIn check_quantities check whether something is a molecular weight quantity
#' @export
is_molecular_weight <- function(q) { is(q, "mk_molecular_weight") }

#' @describeIn check_quantities check whether something is a molarity concentration quantity
#' @export
is_molarity_concentration <- function(q) { is(q, "mk_molarity_concentration") }

#' @describeIn check_quantities check whether something is a mass concentration quantity
#' @export
is_mass_concentration <- function(q) { is(q, "mk_mass_concentration") }

#' @describeIn check_quantities check whether something is a volume quantity
#' @export
is_volume <- function(q) { is(q, "mk_volume") }

#' @describeIn check_quantities check whether something is a pressure quantity
#' @export
is_pressure <- function(q) { is(q, "mk_pressure") }

#' @describeIn check_quantities check whether something is a gas solubility quantity
#' @export
is_gas_solubility <- function(q) { is(q, "mk_gas_solubility") }

#' @describeIn check_quantities check whether something is a temperature quantity
#' @export
is_temperature <- function(q) { is(q, "mk_temperature") }

# type casts: to value (=as.numeric) =====

#' Get quantity data
#'
#' Get data from quantity objects including their values (equivalent of \link[base]{as.numeric}) and text representations (equivalent of \link[base]{as.character}).
#'
#' @name quantity_data
#' @family quantity functions
NULL

#' @param q quantities
#' @param unit which units to retrieve quantity data in (by default the unit the quantity is in)
#' @describeIn quantity_data get the value of a quantity in the desired unit. By default returns the quantity in the units it is in. Functionally equivalent to \code{\link[base]{as.numeric}} and \code{\link[base]{as.double}}.
#' @param transform whether to transform the value with an additional function once in the desired units. Common transformation examples are log10 and log (natural log) but custom transformations are also possible. Default is NO transformation (\link{identity}).
#' @examples
#' # quantity value examples
#' qty(0.1, "g") %>% get_qty_value()
#' qty(0.1, "g") %>% get_qty_value("g")
#' qty(0.1, "g") %>% get_qty_value("g", log10)
#' qty(0, "C") %>% get_qty_value("F")
#' qty(760, "Torr") %>% get_qty_value("atm")
#' @export
get_qty_value <- function(q, unit = get_qty_units(q), transform = identity) {
  vec_cast.double(q, unit = unit, transform = transform)
}

#' @describeIn quantity_data S3 extension of \code{\link[base]{as.numeric}} and \link[base]{as.double} with optional \code{unit} argument
#' @examples
#' qty(760, "Torr") %>% as.numeric("atm")
#' @method as.double mk_quantity
#' @export
as.double.mk_quantity <- function(q, unit = get_qty_units(q), ...) {
  vec_cast.double(q, unit = unit, ...)
}

# helper functions for value retrieval
get_value <- function(q, unit = get_qty_units(q), transform = identity, ...) {
  if (identical(unit, get_qty_units(q))) return(transform(vctrs::vec_data(q)))
  prefix <- get_unit_prefix(unit, get_base_unit(q))
  scaling <- get_metric_scale_factor(q, prefix)
  return(transform(vctrs::vec_data(q) * scaling))
}

get_pressure_value <- function(q, unit = get_qty_units(q), transform = identity, ...) {
  if (identical(unit, get_qty_units(q))) return(transform(vctrs::vec_data(q)))
  unit_conversion <- get_pressure_unit_conversion(unit)
  prefix <- get_unit_prefix(unit_conversion$unit, get_base_unit(q))
  scaling <- get_metric_scale_factor(q, prefix)/unit_conversion$conversion
  return(transform(vctrs::vec_data(q) * scaling))
}

get_gas_solubility_value <- function(q, unit = get_qty_units(q), transform = identity, ...) {
  if (identical(unit, get_qty_units(q))) return(transform(vctrs::vec_data(q)))
  unit_conversion <- get_gas_solubility_unit_conversion(unit)
  prefix <- get_unit_prefix(unit_conversion$unit, get_base_unit(q))
  scaling <- get_metric_scale_factor(q, prefix)/unit_conversion$conversion
  return(transform(vctrs::vec_data(q) * scaling))
}

get_temperature_value <- function(q, unit = get_qty_units(q), transform = identity, ...) {
  if (identical(unit, get_qty_units(q))) return(transform(vctrs::vec_data(q)))
  unit_conversion <- get_temperature_unit_conversion(unit)
  prefix <- get_unit_prefix(unit_conversion$unit, get_base_unit(q))
  scaling <- get_metric_scale_factor(q, prefix)
  return(transform(unit_conversion$conversion_back(vctrs::vec_data(q) * scaling)))
}

#' @method vec_ptype2.double mk_quantity
#' @export
vec_ptype2.double.mk_quantity <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_quantity
#' @export
vec_cast.double.mk_quantity <- function(x, to, ...) get_value(x, ...)

#' @method vec_ptype2.double mk_amount
#' @export
vec_ptype2.double.mk_amount <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_amount
#' @export
vec_cast.double.mk_amount <- function(x, to, ...) {
  get_value(x, ...)
}

#' @method vec_ptype2.double mk_mass
#' @export
vec_ptype2.double.mk_mass <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_mass
#' @export
vec_cast.double.mk_mass <- function(x, to, ...) {
  get_value(x, ...)
}

#' @method vec_ptype2.double mk_molecular_weight
#' @export
vec_ptype2.double.mk_molecular_weight <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_molecular_weight
#' @export
vec_cast.double.mk_molecular_weight <- function(x, to, ...) get_value(x, ...)

#' @method vec_ptype2.double mk_molarity_concentration
#' @export
vec_ptype2.double.mk_molarity_concentration <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_molarity_concentration
#' @export
vec_cast.double.mk_molarity_concentration <- function(x, to, ...) {
  get_value(x, ...)
}

#' @method vec_ptype2.double mk_mass_concentration
#' @export
vec_ptype2.double.mk_mass_concentration <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_mass_concentration
#' @export
vec_cast.double.mk_mass_concentration <- function(x, to, ...) {
  get_value(x, ...)
}

#' @method vec_ptype2.double mk_volume
#' @export
vec_ptype2.double.mk_volume <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_volume
#' @export
vec_cast.double.mk_volume <- function(x, to, ...) {
  get_value(x, ...)
}

#' @method vec_ptype2.double mk_pressure
#' @export
vec_ptype2.double.mk_pressure <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_pressure
#' @export
vec_cast.double.mk_pressure <- function(x, to, ...) {
  get_pressure_value(x, ...)
}

#' @method vec_ptype2.double mk_gas_solubility
#' @export
vec_ptype2.double.mk_gas_solubility <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_gas_solubility
#' @export
vec_cast.double.mk_gas_solubility <- function(x, to, ...) {
  get_gas_solubility_value(x, ...)
}

#' @method vec_ptype2.double mk_temperature
#' @export
vec_ptype2.double.mk_temperature <- function(x, y, ..., x_arg = "x", y_arg = "y") double()
#' @method vec_cast.double mk_temperature
#' @export
vec_cast.double.mk_temperature <- function(x, to, ...) {
  get_temperature_value(x, ...)
}

# type casts: to text (=as.character) =====

#' @describeIn quantity_data get the value of the quantity in the desired unit as a text string with the unit appended. Functionally equivalent to \code{\link[base]{as.character}}.
#' @param signif number of significant digits for printing the quantity
#' @export
#' @examples
#'
#' # quantity text examples
#' qty(0.1, "g") %>% get_qty_text()
#' qty(0.1, "g") %>% get_qty_text("g")
#' qty(0:10, "C") %>% get_qty_text("F")
#' qty(760, "Torr") %>% get_qty_text("atm")
get_qty_text <- function(q, unit = get_qty_units(q), signif = 5) {
  return(get_text(q, unit, signif))
}

#' @describeIn quantity_data S3 implementation of \code{\link[base]{as.character}} with optional \code{unit} argument
#' @examples
#' qty(760, "Torr") %>% as.character("atm")
#' @method as.character mk_quantity
#' @export
as.character.mk_quantity <- function(q, unit = get_qty_units(q), ...) {
  vec_cast.character(q, unit = unit, ...)
}

#' @describeIn quantity_data get each value of a quantity in the best metric unit with the unit appended. Note that if a value is zero, it will use the unit of the next smallest value for this number.
#' @export
#' @examples
#'
#' # each quantity text example
#' qty(c(0, 0.1, 10, 1000), "mg") %>% get_qty_text() # 0.1mg 10mg 1000mg
#' qty(c(0, 0.1, 10, 1000), "mg") %>% get_qty_text_each() # 0ug 100ug 10mg 1g
#' @export
get_qty_text_each <- function(q, signif = 5) {
  is_zero <- is_near_zero(q)
  smallest_non_zero <- min(abs(q[!is_zero]))
  nearest_zero_unit <- get_qty_units(best_metric(q[which(abs(q) == smallest_non_zero)[1]]))
  text <- purrr::map_chr(q, ~get_text(best_metric(.x), signif = signif))
  text[is_zero] <- get_text(q[is_zero], nearest_zero_unit)
  return(text)
}

# helper function for getting text representation
get_text <- function(q, unit = get_qty_units(q), signif = 5, ...) {
  text <- sprintf("%s %s", base::signif(get_qty_value(q, unit = unit), signif), unit)
  text[is.na(q)] <- NA_character_
  return(text)
}

#' @method vec_ptype2.character mk_quantity
#' @export
vec_ptype2.character.mk_quantity <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_quantity
#' @export
vec_cast.character.mk_quantity <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_amount
#' @export
vec_ptype2.character.mk_amount <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_amount
#' @export
vec_cast.character.mk_amount <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_mass
#' @export
vec_ptype2.character.mk_mass <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_mass
#' @export
vec_cast.character.mk_mass <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_molecular_weight
#' @export
vec_ptype2.character.mk_molecular_weight <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_molecular_weight
#' @export
vec_cast.character.mk_molecular_weight <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_molarity_concentration
#' @export
vec_ptype2.character.mk_molarity_concentration <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_molarity_concentration
#' @export
vec_cast.character.mk_molarity_concentration <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_mass_concentration
#' @export
vec_ptype2.character.mk_mass_concentration <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_mass_concentration
#' @export
vec_cast.character.mk_mass_concentration <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_volume
#' @export
vec_ptype2.character.mk_volume <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_volume
#' @export
vec_cast.character.mk_volume <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_pressure
#' @export
vec_ptype2.character.mk_pressure <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_pressure
#' @export
vec_cast.character.mk_pressure <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_gas_solubility
#' @export
vec_ptype2.character.mk_gas_solubility <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_gas_solubility
#' @export
vec_cast.character.mk_gas_solubility <- function(x, to, ...) { get_text(x, ...) }

#' @method vec_ptype2.character mk_temperature
#' @export
vec_ptype2.character.mk_temperature <- function(x, y, ..., x_arg = "x", y_arg = "y") character()
#' @method vec_cast.character mk_temperature
#' @export
vec_cast.character.mk_temperature <- function(x, to, ...) { get_text(x, ...) }

# type casts: to factor ====

#' @importFrom generics as.factor
#' @export
generics::as.factor

#' @method as.factor mk_quantity
#' @export
as.factor.mk_quantity <- function(x, unit = get_qty_units(x), ...) {
  x <- as.character(x, unit = unit)
  return(generics::as.factor(x, ...))
}


#' @importFrom forcats as_factor
#' @export
forcats::as_factor

#' @method as_factor mk_quantity
#' @export
as_factor.mk_quantity <- function(x, unit = get_qty_units(x), ...) {
  x <- as.character(x, unit = unit)
  forcats::as_factor(x, ...)
}

# type casts: to same quantity type (=c) ====

#' Concatenate quantities
#'
#' Concatenate multiple quantity vectors or values. They must all be of the same type (i.e. you cannot combine e.g. a temperature and a mass value). The concatenated values will be scaled according to \code{\link{best_metric}}. Note that the regular `c()` operator automatically calls this function if the first argument is a quantity object.
#' @param ... \link{quantities} to concatenate
#' @examples
#' c_qty(qty(5, "g"), qty(c(10, 20), "mg")) # MediaChemToolsMass [mg]: 5000, 10, 20
#' c(qty(5, "g"), qty(c(10, 20), "mg")) # same (shortcut for the above)
#' @export
c_qty <- function(...) {
  c(...)
}

# preserve quantity info on combination
#' @export
c.mk_quantity <- function(...) {
  best_metric(vctrs::vec_c(...))
}

#' vec_ptype2 for microbial kitchen objects
#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_quantity
#' @export
#' @export vec_ptype2.mk_quantity
vec_ptype2.mk_quantity <- function(x, y, ...)
  UseMethod("vec_ptype2.mk_quantity", y)


#' @method vec_ptype2.mk_quantity default
#' @export
vec_ptype2.mk_quantity.default <- function(x, y, ..., x_arg = "x", y_arg = "y")
  vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

#' vec_cast for mk_quantity objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_quantity
#' @export
#' @export vec_cast.mk_quantity
vec_cast.mk_quantity <- function(x, to, ...)
  UseMethod("vec_cast.mk_quantity")

#' @method vec_cast.mk_quantity default
#' @export
vec_cast.mk_quantity.default <- function(x, to, ...)
  vctrs::vec_default_cast(x, to)

# note: do not allow combination of base quantity objects because units are not standardized

#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_amount
#' @export
#' @export vec_ptype2.mk_amount
vec_ptype2.mk_amount <- function(x, y, ...) UseMethod("vec_ptype2.mk_amount", y)
#' @method vec_ptype2.mk_amount default
#' @export
vec_ptype2.mk_amount.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_amount mk_amount
#' @export
vec_ptype2.mk_amount.mk_amount <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_amount objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_amount
#' @export
#' @export vec_cast.mk_amount
vec_cast.mk_amount <- function(x, to, ...)
  UseMethod("vec_cast.mk_amount")

#' @method vec_cast.mk_amount default
#' @export
vec_cast.mk_amount.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_amount mk_amount
#' @export
vec_cast.mk_amount.mk_amount <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}

#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_mass
#' @export
#' @export vec_ptype2.mk_mass
vec_ptype2.mk_mass <- function(x, y, ...) UseMethod("vec_ptype2.mk_mass", y)
#' @method vec_ptype2.mk_mass default
#' @export
vec_ptype2.mk_mass.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_mass mk_mass
#' @export
vec_ptype2.mk_mass.mk_mass <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_mass objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_mass
#' @export
#' @export vec_cast.mk_mass
vec_cast.mk_mass <- function(x, to, ...)
  UseMethod("vec_cast.mk_mass")

#' @method vec_cast.mk_mass default
#' @export
vec_cast.mk_mass.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_mass mk_mass
#' @export
vec_cast.mk_mass.mk_mass <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_molecular_weight
#' @export
#' @export vec_ptype2.mk_molecular_weight
vec_ptype2.mk_molecular_weight <- function(x, y, ...) UseMethod("vec_ptype2.mk_molecular_weight", y)
#' @method vec_ptype2.mk_molecular_weight default
#' @export
vec_ptype2.mk_molecular_weight.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_molecular_weight mk_molecular_weight
#' @export
vec_ptype2.mk_molecular_weight.mk_molecular_weight <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_molecular_weight objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_molecular_weight
#' @export
#' @export vec_cast.mk_molecular_weight
vec_cast.mk_molecular_weight <- function(x, to, ...)
  UseMethod("vec_cast.mk_molecular_weight")

#' @method vec_cast.mk_molecular_weight default
#' @export
vec_cast.mk_molecular_weight.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_molecular_weight mk_molecular_weight
#' @export
vec_cast.mk_molecular_weight.mk_molecular_weight <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_molarity_concentration
#' @export
#' @export vec_ptype2.mk_molarity_concentration
vec_ptype2.mk_molarity_concentration <- function(x, y, ...) UseMethod("vec_ptype2.mk_molarity_concentration", y)
#' @method vec_ptype2.mk_molarity_concentration default
#' @export
vec_ptype2.mk_molarity_concentration.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_molarity_concentration mk_molarity_concentration
#' @export
vec_ptype2.mk_molarity_concentration.mk_molarity_concentration <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_molarity_concentration objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_molarity_concentration
#' @export
#' @export vec_cast.mk_molarity_concentration
vec_cast.mk_molarity_concentration <- function(x, to, ...)
  UseMethod("vec_cast.mk_molarity_concentration")

#' @method vec_cast.mk_molarity_concentration default
#' @export
vec_cast.mk_molarity_concentration.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_molarity_concentration mk_molarity_concentration
#' @export
vec_cast.mk_molarity_concentration.mk_molarity_concentration <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_mass_concentration
#' @export
#' @export vec_ptype2.mk_mass_concentration
vec_ptype2.mk_mass_concentration <- function(x, y, ...) UseMethod("vec_ptype2.mk_mass_concentration", y)
#' @method vec_ptype2.mk_mass_concentration default
#' @export
vec_ptype2.mk_mass_concentration.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_mass_concentration mk_mass_concentration
#' @export
vec_ptype2.mk_mass_concentration.mk_mass_concentration <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_mass_concentration objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_mass_concentration
#' @export
#' @export vec_cast.mk_mass_concentration
vec_cast.mk_mass_concentration <- function(x, to, ...)
  UseMethod("vec_cast.mk_mass_concentration")

#' @method vec_cast.mk_mass_concentration default
#' @export
vec_cast.mk_mass_concentration.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_mass_concentration mk_mass_concentration
#' @export
vec_cast.mk_mass_concentration.mk_mass_concentration <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_volume
#' @export
#' @export vec_ptype2.mk_volume
vec_ptype2.mk_volume <- function(x, y, ...) UseMethod("vec_ptype2.mk_volume", y)
#' @method vec_ptype2.mk_volume default
#' @export
vec_ptype2.mk_volume.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_volume mk_volume
#' @export
vec_ptype2.mk_volume.mk_volume <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_volume objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_volume
#' @export
#' @export vec_cast.mk_volume
vec_cast.mk_volume <- function(x, to, ...)
  UseMethod("vec_cast.mk_volume")

#' @method vec_cast.mk_volume default
#' @export
vec_cast.mk_volume.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_volume mk_volume
#' @export
vec_cast.mk_volume.mk_volume <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_pressure
#' @export
#' @export vec_ptype2.mk_pressure
vec_ptype2.mk_pressure <- function(x, y, ...) UseMethod("vec_ptype2.mk_pressure", y)
#' @method vec_ptype2.mk_pressure default
#' @export
vec_ptype2.mk_pressure.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_pressure mk_pressure
#' @export
vec_ptype2.mk_pressure.mk_pressure <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_pressure objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_pressure
#' @export
#' @export vec_cast.mk_pressure
vec_cast.mk_pressure <- function(x, to, ...)
  UseMethod("vec_cast.mk_pressure")

#' @method vec_cast.mk_pressure default
#' @export
vec_cast.mk_pressure.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_pressure mk_pressure
#' @export
vec_cast.mk_pressure.mk_pressure <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_gas_solubility
#' @export
#' @export vec_ptype2.mk_gas_solubility
vec_ptype2.mk_gas_solubility <- function(x, y, ...) UseMethod("vec_ptype2.mk_gas_solubility", y)
#' @method vec_ptype2.mk_gas_solubility default
#' @export
vec_ptype2.mk_gas_solubility.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_gas_solubility mk_gas_solubility
#' @export
vec_ptype2.mk_gas_solubility.mk_gas_solubility <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_gas_solubility objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_gas_solubility
#' @export
#' @export vec_cast.mk_gas_solubility
vec_cast.mk_gas_solubility <- function(x, to, ...)
  UseMethod("vec_cast.mk_gas_solubility")

#' @method vec_cast.mk_gas_solubility default
#' @export
vec_cast.mk_gas_solubility.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_gas_solubility mk_gas_solubility
#' @export
vec_cast.mk_gas_solubility.mk_gas_solubility <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


#' @rdname vec_ptype2
#' @inheritParams vctrs::vec_ptype2
#' @method vec_ptype2 mk_temperature
#' @export
#' @export vec_ptype2.mk_temperature
vec_ptype2.mk_temperature <- function(x, y, ...) UseMethod("vec_ptype2.mk_temperature", y)
#' @method vec_ptype2.mk_temperature default
#' @export
vec_ptype2.mk_temperature.default <- function(x, y, ..., x_arg = "x", y_arg = "y") vctrs::vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
#' @method vec_ptype2.mk_temperature mk_temperature
#' @export
vec_ptype2.mk_temperature.mk_temperature <- function(x, y, ..., x_arg = "x", y_arg = "y") x

#' vec_cast for mk_temperature objects
#' @rdname vec_cast
#' @inheritParams vctrs::vec_cast
#' @method vec_cast mk_temperature
#' @export
#' @export vec_cast.mk_temperature
vec_cast.mk_temperature <- function(x, to, ...)
  UseMethod("vec_cast.mk_temperature")

#' @method vec_cast.mk_temperature default
#' @export
vec_cast.mk_temperature.default <- function(x, to, ...) vctrs::vec_default_cast(x, to)

#' @method vec_cast.mk_temperature mk_temperature
#' @export
vec_cast.mk_temperature.mk_temperature <- function(x, to, ...) {
  scale_metric(x, prefix = get_prefix(to))
}


# metric conversions ======================

# check if a quantity is near zero
is_near_zero <- function(q) {
  # don't support numbers smaller than 0.001 femto
  abs(vctrs::vec_data(scale_metric(q, "f"))) < 1e-3
}

# get all metric prefixes
get_metric_prefixes <- function() {
  get_microbialkitchen_constant("metric_prefix")
}

#' Metric prefixes
#'
#' These functions simplify scaling between different metric prefixes.
#'
#' @name metric_scaling
NULL

# convenience function to determine metric scaling factor
get_metric_scale_factor <- function(q, prefix) {
  metric_prefix <- get_metric_prefixes()
  if (!is_qty(q)) stop("not a known type of quantity: ", class(q))
  if (! prefix %in% names(metric_prefix)) stop("not a known metric prefix: ", prefix)
  q_prefix <- get_prefix(q)
  if (q_prefix == prefix) return(1) # already the requested metric (for speed)

  # conversion
  scale_factor <- metric_prefix[[which(names(metric_prefix)==q_prefix)]]/ # complication required because of unity unit with "" name
    metric_prefix[[which(names(metric_prefix)==prefix)]]
  return(scale_factor)
}

#' @describeIn metric_scaling scale to a specific metrix prefix (from whatever the quantity is currently in)
#' @param q the \link{quantity} to scale
#' @param prefix a metric prefix (p, n, µ, m, k, M, etc.)
#' @family quantity functions
#' @export
scale_metric <- function (q, prefix = "") {
  scale_factor <- get_metric_scale_factor(q, prefix)
  q[] <- scale_factor * vctrs::vec_data(q)
  attr(q, "unit") <- paste0(prefix, get_base_unit(q))
  return(q)
}

#' @describeIn metric_scaling convert to best metric prefix (i.e. one that gives at least 1 significant digit before the decimal),
#' if the quantity has a vector of values, scales to the best metric prefix for the median of all values
#' @export
best_metric <- function(q) {
  if (!is_qty(q)) stop("not a known type of quantity: ", class(q)[1], call. = FALSE)
  is_zero <- is_near_zero(q)
  q[is_zero] <- 0.0
  if(all(is.na(q) | is_zero | is.infinite(q))) return(q)
  prefix <- get_best_metric_prefix(vctrs::vec_data(base_metric(q)))
  return(scale_metric(q, prefix = prefix))
}

# helper function to get the best metric prefix
get_best_metric_prefix <- function(x) {
  prefix <- get_metric_prefixes()
  if (length(x) == 0 || all(is.na(x) | is.infinite(x))) {
    ideal <- which(names(prefix) == "")
  } else {
    x <- x[!is.infinite(x)]
    ideal <- max(1, which( stats::median(abs(x), na.rm = TRUE)/prefix >= 1))
  }
  return(names(prefix)[ideal])
}


#' @describeIn metric_scaling convert to base metric prefix of microbialkitchen (i.e. to mol, L, etc.)
#' @export
base_metric <- function(q) {
  if (!is_qty(q)) stop("not a known type of quantity: ", class(q)[1])
  if (get_prefix(q) == "") return(q) # already base metric (faster this way)
  else return(scale_metric(q, prefix = ""))
}

# Get all base units
get_base_units <- function() {
  get_microbialkitchen_constant("base_units")
}

# base unit by qty class name (used in constructor defaults)
base_unit <- function(q_type) {
  return(unname(get_base_units()[q_type]))
}

# Get the base unit of a quantity
get_base_unit <- function(q) {
  return(base_unit(get_qty_class(q)))
}

# Get the prefix of a quantity
get_prefix <- function(q) {
  return(get_unit_prefix(attr(q, "unit"), get_base_unit(q)))
}

# Get the prefix from a unit
get_unit_prefix <- function(unit, base_unit) {
  if (! grepl(paste0(base_unit, "$"), unit)) {
    sprintf("not a valid unit for this quantity (base unit '%s'): %s", base_unit, unit) %>%
      stop(call. = FALSE)
  }
  return(sub(paste0(base_unit, "$"), "", unit))
}
