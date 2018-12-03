#' @include classes.R
NULL

# quantities ======

#' Quantities
#' 
#' The \code{qty} function makes it easy to keep track of different quantities in chemical calculations. Metric prefixes are fully supported, i.e. any unit can be combined with standard \link{metric} scaling (mL, nmol, µM, etc.). Some quantities can also be used in common \link{arithmetic} operations.
#' @name quantities
NULL

#' @describeIn quantities generate a quantity object
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
  if (!is.null(r <- tryCatch(molarity(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(density(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(volume(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(amount(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(mass(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(pressure(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(temperature(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(molecular_weight(x, unit, scale_to_best_metric), error = function(e){}))) return(r)
  stop("Could not determine the appropriate quantity for unit ", unit)
}

#' @details \emph{amount}: base unit \code{mol} but also understands \code{mole}, all metric prefixes allowed
#' @name quantities
NULL
amount <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "mol")
  secondary_units <- paste0(names(prefix), "mole")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known amount unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("MediaToolsAmount", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{mass}: base unit \code{g}, all metric prefixes allowed
#' @name quantities
NULL
mass <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "g")
  if (! unit %in% primary_units) stop("not a known mass unit: ", unit)
  
  q <- new("MediaToolsMass", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{molecular weight}: base unit \code{g/mol}, all metric prefixes allowed in the numerator
#' @name quantities
NULL
molecular_weight <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "g/mol")
  secondary_units <- paste0(names(prefix), "Da")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known molecular weight unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("MediaToolsMolecularWeight", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{concentration (molarity)}: base unit \code{M} but also understands \code{mol/L}, all metric prefixes allowed in the numerator
#' @name quantities
NULL
molarity <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "M")
  secondary_units <- paste0(names(prefix), "mol/L")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known concentration (molarity) unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("MediaToolsMolarity", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{concentration (density)}: base unit \code{g/L} but also understands \code{g/l}, all metric prefixes allowed in the numerator
#' @name quantities
NULL
density <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "g/L")
  secondary_units <- paste0(names(prefix), "g/l")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known concentration (density) unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("MediaToolsDensity", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{volume}: base unit \code{L} but also understands \code{l}, all metric prefixes allowed
#' @name quantities
NULL
volume <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "L")
  secondary_units <- paste0(names(prefix), "l")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known volume unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("MediaToolsVolume", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{pressure}: base unit \code{bar} but also understands \code{Pa}, all metric prefixes allowed, the common non-metric units \code{atm} and \code{psi}, \code{Torr} and \code{mTorr} are also supported and will be automatically converted to \code{bar}
#' @name quantities
NULL
pressure <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
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
    c_factor <- get_mediatools_constant(paste0("bar_per_", unit))
    x <- x * c_factor
    unit <- "bar"
  }
  
  # pascal
  if (unit %in% secondary_units) {
    x <- x * get_mediatools_constant("bar_per_pa")
    unit <- primary_units[secondary_units == unit]
  }
  
  q <- new("MediaToolsPressure", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

#' @details \emph{temperature}: base unit \code{K} but also understands \code{C} and \code{F} and converts them to Kelvin
#' @name quantities
NULL
temperature <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "K")
  alternative_units <- c("C", "F")
  if (! unit %in% c(primary_units, alternative_units)) 
    stop("not a known temperature unit: ", unit)
  
  # alternative units
  if (unit == "C") {
    x <- x - get_mediatools_constant("celsius_kelvin_offset")
    unit <- "K"
  } else if (unit == "F") {
    x <- (x - get_mediatools_constant("fahrenheit_celsius_offset"))/get_mediatools_constant("fahrenheit_celsius_slope") - 
      get_mediatools_constant("celsius_kelvin_offset") 
    unit <- "K"
  }
  
  q <- new("MediaToolsTemperature", x, unit = unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# S4 methods ========================

# allow quantity replication
#' @export
rep.MediaToolsQuantity <- function(x, ...) {
  x@.Data <- rep(x@.Data, ...)
  return(x)
}

# preserve quantity info on subsetting
#' @export
`[.MediaToolsQuantity` <- function(x, ...) {
  x@.Data <- `[`(x@.Data, ...)
  return(x)
}

# preserve quantity info on subsetting
#' @export
`[[.MediaToolsQuantity` <- function(x, ...) {
  x@.Data <- `[[`(x@.Data, ...)
  return(x)
}

# make sure the units are displayed inside dplyr data frame representations
# note: not using shorts to keep the types easier defined
# note: this is not yet supported in paged tables (i.e. knitted data frames display) because rmarkdown:::paged_table_type_sum does not use tibble::type_sum
#' @export
type_sum.MediaToolsQuantity <- function(x) {
  # shorts <- c(MediaToolsAmount = "amt", MediaToolsMass = "m", MediaToolsMolecularWeight = "MW", MediaToolsMolarity = "C",
  #             MediaToolsDensity = "C", MediaToolsVolume = "V", MediaToolsPressure = "P", MediaToolsTemperature = "T")
  # qclass <- class(x)[1]
  # if (!qclass %in% names(shorts))
  #   stop("no type sum short available for the objects of type ", qclass, call. = FALSE)
  # sprintf("%s [%s]", shorts[qclass], x@unit)
  sprintf("[%s]", x@unit)
}


#' Concatenate quantities
#' 
#' Concatenate multiple quantity vectors or values. They must all be of the same type (i.e. you cannot combine e.g. a temperature and a mass value). The concatenated values will be scaled according to \code{\link{best_metric}}. Note that the regular `c()` operator automatically calls this function if the first argument is a quantity object.
#' @examples 
#' c_qty(qty(5, "g"), qty(c(10, 20), "mg")) # MediaToolsMass [mg]: 5000, 10, 20
#' c(qty(5, "g"), qty(c(10, 20), "mg")) # same (shortcut for the above)
#' @export
c_qty <- function(...) {
  qs <- list(...)
  # safety check that all quantities are the same classes
  classes <- purrr::map_chr(qs, ~class(.x)[1])
  if (any(classes != classes[1])) {
    stop(sprintf("cannot combine different types quantities (trying to combine %s). ", 
                 glue::glue_collapse(unique(classes), sep = ", ", last = " and ")), call. = FALSE)
  }
  # combine quantities making sure metric scaling is appropriate
  purrr::map(qs, ~as.numeric(base_metric(.x))) %>% 
    unlist() %>% 
    qty(get_base_unit(qs[[1]]))
}

# preserve quantity info on combination
#' @export
c.MediaToolsQuantity <- function(...) {
  c_qty(...)
}

# unit retrieval ====================

#' @describeIn quantities get units from a quantity or list of quantities (returns NA for objects that are not quantities)
#' @param q quantity or list of quantities
#' @export
get_qty_units <- function(q) {
  if (methods::is(q, "MediaToolsQuantity")) 
    return(q@unit)
  else if (is.list(q))
    return(purrr::map_chr(q, ~if(methods::is(.x, "MediaToolsQuantity")) { .x@unit } else { NA_character_ }))
  else
    return(NA_character_)
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

  metric_prefix <- get_mediatools_constant("metric_prefix")
  if (!inherits(q, "MediaToolsQuantity")) stop("not a known type of quantity: ", class(q))
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
  prefix <- get_mediatools_constant("metric_prefix")
  ideal <-
    if (length(q) == 0 || all(is.na(q)) || all(is.infinite(q))) which(names(prefix) == "")
    else max(1, which( median(abs(as.numeric(base_metric(q))), na.rm = TRUE)/prefix >= 1))
  scale_metric(q, names(prefix)[ideal])
}


#' @describeIn metric convert to base metric prefix of mediatools (i.e. to mol, L, etc.)
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
  
