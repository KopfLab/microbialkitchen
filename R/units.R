#' Quantities
#' 
#' These functions make it easy to keep track of different quantities in
#' chemical calculations. Metric prefixes are fully supported, i.e. any
#' unit can be combined with standard \link{metric} scaling (mL, nmol, µM, etc.)
#' @name quantities
#' @aliases quantity
NULL

#' @describeIn quantities universal function for generating a quantity (will try to guess which type from the unit)
#' @param x the numeric value of the quantity, can be a single value or a vector
#' @param unit the unit of the quantity
#' @param scale_to_best_metric whether to automatically scale to the best metric prefix
#' @examples
#' qty(0.045, "mmol/L")
#' qty(6, "psi")
#' qty(30, "C")
#' @export
#' @family functions
qty <- function(x, unit, scale_to_best_metric = TRUE) {
  if (!is.null(r <- tryCatch(concentration(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(volume(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(amount(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(mass(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(pressure(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(temperature(x, unit), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(molecular_weight(x, unit, FALSE), error = function(e){}))) return(r)
  stop("Could not determine the appropriate quantity for unit ", unit)
}

#' @describeIn quantities generate an amount (base unit \code{mol} but also understands \code{mole}, 
#' all metric prefixes allowed)
#' @export
amount <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "mol")
  secondary_units <- paste0(names(prefix), "mole")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known amount unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("Amount", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @describeIn quantities generate a mass (base unit \code{g}, 
#' all metric prefixes allowed)
#' @export
mass <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "g")
  if (! unit %in% primary_units) stop("not a known mass unit: ", unit)
  
  q <- new("Mass", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @describeIn quantities generate a molecular weight (base unit \code{g/mol}, 
#' all metric prefixes allowed in the numerator)
#' @export
molecular_weight <- function(x, unit, scale_to_best_metric = FALSE) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "g/mol")
  if (! unit %in% primary_units) stop("not a known molecular weight unit: ", unit)
  
  q <- new("MolecularWeight", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @describeIn quantities generate a molarity (base unit \code{M} but also understands \code{mol/L}, 
#' all metric prefixes allowed in the numerator)
#' @export
concentration <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "M")
  secondary_units <- paste0(names(prefix), "mol/L")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known concentration unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("Molarity", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @describeIn quantities generate a volume (base unit \code{L} but also understands \code{l}, 
#' all metric prefixes allowed)
#' @export
volume <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "L")
  secondary_units <- paste0(names(prefix), "l")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known volume unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("Volume", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @describeIn quantities generate a pressure (base unit \code{bar} but also understands \code{Pa}, 
#' all metric prefixes allowed, the common non-metric units \code{atm} and \code{psi}, \code{Torr} and
#' \code{mTorr} are also supported and will be automatically converted to \code{bar}). 
#' @export
pressure <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "bar")
  secondary_units <- paste0(names(prefix), "Pa")
  alternative_units <- c("atm", "psi", "Torr", "mTorr")
  if (! unit %in% c(primary_units, secondary_units, alternative_units)) 
    stop("not a known pressure unit: ", unit)
  
  # alternative units
  if (unit == "mTorr") {
    x <- x/1000
    unit <- "Torr"
  }
  
  if (unit %in% alternative_units) {
    c_factor <- ct_get_constant(paste0("bar_per_", unit))
    x <- x * c_factor
    unit <- "bar"
  }
  
  # pascal
  if (unit %in% secondary_units) {
    x <- x * ct_get_constant("bar_per_pa")
    unit <- primary_units[secondary_units == unit]
  }
  
  q <- new("Pressure", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @describeIn quantities generate a temperature (base unit \code{K} but also understands
#' \code{C} and \code{F} and converts them to Kelvin)
#' @export
temperature <- function(x, unit) {
  prefix <- ct_get_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "K")
  alternative_units <- c("C", "F")
  if (! unit %in% c(primary_units, alternative_units)) 
    stop("not a known temperature unit: ", unit)
  
  # alternative units
  if (unit == "C") {
    x <- x - ct_get_constant("celsius_kelvin_offset")
    unit <- "K"
  } else if (unit == "F") {
    x <- (x - ct_get_constant("fahrenheit_celsius_offset"))/ct_get_constant("fahrenheit_celsius_slope") - 
      ct_get_constant("celsius_kelvin_offset") 
    unit <- "K"
  }
  
  new("Temperature", x, unit = unit)
}

# metric conversions ======================

#' Metric prefixes
#' 
#' These functions simplify converting between different metric prefixes.
#' 
#' @name metric
NULL

#' @describeIn metric scale to a specific metrix prefix (from whatever the quantity is currently in)
#' @param q the \link{quantity} to scale
#' @param prefix a metric prefix (p, n, µ, m, k, M, etc.)
#' @family functions
#' @export
scale_metric <- function (q, prefix = "") {

  metric_prefix <- ct_get_constant("metric_prefix")
  if (!inherits(q, "Quantity")) stop("not a known type of quantity: ", class(q))
  if (! prefix %in% names(metric_prefix)) stop("not a known metric prefix: ", prefix)
  q_prefix <- get_prefix(q)
  
  # conversion
  scale_factor <- metric_prefix[[which(names(metric_prefix)==q_prefix)]]/ # complication required because of unity unit with "" name
               metric_prefix[[which(names(metric_prefix)==prefix)]]
  q@.Data <- scale_factor * q@.Data
  q@unit <- paste0(prefix, get_base_unit(q))
  return(q)
}

#' @describeIn metric convert to best metric prefix (i.e. one that gives at least 1 significant digit before the decimal), 
#' if the quantity has a vector of values, scales to the best metric prefix for the median of all values
#' @export
best_metric <- function(q) {
  prefix <- ct_get_constant("metric_prefix")
  ideal <- max(1, which( median(abs(base_metric(q)))/prefix >= 1))
  scale_metric(q, names(prefix)[ideal])
}


#' @describeIn metric convert to base metric prefix of chemtools (i.e. to mol, L, etc.)
#' @export
base_metric <- function(q) {
  scale_metric(q, prefix = "")
}

# Get the base unit of a quantiy
get_base_unit <- function(q) {
  return(new(class(q))@unit)
}

# Get the prefix of a quantity
get_prefix <- function(q) {
  base_unit <- get_base_unit(q)
  if (! grepl(paste0(base_unit, "$"), q@unit)) 
    stop("not a valid unit: ", q@unit) # this should never happen
  return(sub(paste0(base_unit, "$"), "", q@unit))
}
  
