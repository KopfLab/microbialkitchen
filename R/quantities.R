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
#' @family quantities
qty <- function(x, unit, scale_to_best_metric = TRUE) {
  if (!is(r <- try(molarity(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(density(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(volume(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(amount(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(mass(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(pressure(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(solubility(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(temperature(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
  if (!is(r <- try(molecular_mass(x, unit, scale_to_best_metric), silent = TRUE), "try-error")) return(r)
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

#' @details \emph{molecular mass}: base unit \code{g/mol}, all metric prefixes allowed in the numerator
#' @name quantities
NULL
molecular_mass <- function(x, unit, scale_to_best_metric = TRUE) {
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), "g/mol")
  secondary_units <- paste0(names(prefix), "Da")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known molecular mass unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  q <- new("MediaToolsMolecularMass", x, unit = unit)
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

#' @details \emph{pressure}: base unit \code{bar} but also understands \code{Pa}, all metric prefixes allowed, the common non-metric units \code{atm}, \code{psi}, \code{Torr}, \code{mTorr}, and \code{% SP} (% at standard pressure = % of 1 bar) are also supported and will be automatically converted to \code{bar}.
#' @name quantities
NULL
pressure <- function(x, unit, scale_to_best_metric = TRUE) {
  unit_conversion <- get_pressure_unit_conversion(unit)
  q <- new("MediaToolsPressure", x * unit_conversion$conversion, unit = unit_conversion$unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# get conversion for pressure units
get_pressure_unit_conversion <- function(unit) {
  conversion <- 1
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), get_base_unit(new("MediaToolsPressure")))
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
    c_factor <- get_mediatools_constant(paste0("bar_per_", unit))
    conversion <- conversion * c_factor
    unit <- "bar"
  }
    
  # pascal
  if (unit %in% secondary_units) {
    conversion <- conversion * get_mediatools_constant("bar_per_pa")
    unit <- primary_units[secondary_units == unit]
  }
  
  return(list(unit = unit, conversion = conversion))
}

#' @details \emph{Henry's law solubility constant}: base unit \code{M/bar}, all metric prefixes allowed in the numerator, the common non-metric unit \code{M/atm} is also supported and will be automatically converted to \code{M/bar}.
#' @name quantities
NULL
solubility <- function(x, unit, scale_to_best_metric = TRUE) {
  unit_conversion <- get_solubility_unit_conversion(unit)
  q <- new("MediaToolsSolubility", x * unit_conversion$conversion, unit = unit_conversion$unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# get conversion for solubility units
get_solubility_unit_conversion <- function(unit) {
  conversion <- 1
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), get_base_unit(new("MediaToolsSolubility")))
  secondary_units <- paste0(names(prefix), "M/atm")
  if (! unit %in% c(primary_units, secondary_units)) 
    stop("not a known solubility unit: ", unit)
  
  # alternative units
  if (unit %in% secondary_units) {
    c_factor <- get_mediatools_constant("bar_per_atm")
    conversion <- conversion / c_factor
    unit <- primary_units[secondary_units == unit]
  }
  
  return(list(unit = unit, conversion = conversion))
}

#' @details \emph{temperature}: base unit \code{K} but also understands \code{C} and \code{F} and converts them to Kelvin
#' @name quantities
NULL
temperature <- function(x, unit, scale_to_best_metric = TRUE) {
  unit_conversion <- get_temperature_unit_conversion(unit)
  q <- new("MediaToolsTemperature", unit_conversion$conversion_fwd(x), unit = unit_conversion$unit)
  if (scale_to_best_metric) q <- best_metric(q)
  return(q)
}

# get conversion for temperature units
get_temperature_unit_conversion <- function(unit) {
  conversion_fwd <- function(x) x
  conversion_back <- function(x) x
  prefix <- get_mediatools_constant("metric_prefix")
  primary_units <- paste0(names(prefix), get_base_unit(new("MediaToolsTemperature")))
  alternative_units <- c("C", "F")
  if (! unit %in% c(primary_units, alternative_units)) 
    stop("not a known temperature unit: ", unit)
  
  # alternative units
  if (unit == "C") {
    conversion_fwd <- function(x) x - get_mediatools_constant("celsius_kelvin_offset")
    conversion_back <- function(x) x + get_mediatools_constant("celsius_kelvin_offset")
    unit <- "K"
  } else if (unit == "F") {
    conversion_fwd <- function(x) {
      (x - get_mediatools_constant("fahrenheit_celsius_offset"))/get_mediatools_constant("fahrenheit_celsius_slope") - 
        get_mediatools_constant("celsius_kelvin_offset") 
    }
    conversion_back <- function(x) {
      (x + get_mediatools_constant("celsius_kelvin_offset")) * get_mediatools_constant("fahrenheit_celsius_slope") + 
        get_mediatools_constant("fahrenheit_celsius_offset")
    }
    unit <- "K"
  }
  
  return(list(unit = unit, conversion_fwd = conversion_fwd, conversion_back = conversion_back))
}

# type checks =================

#' @describeIn quantities check whether something is a quantity
#' @param q a quantity object
#' @export
is_qty <- function(q) {
  is(q, "MediaToolsQuantity")
}

#' @describeIn quantities check whether something is an amount quantity
#' @export
is_amount <- function(q) is(q, "MediaToolsAmount")

#' @describeIn quantities check whether something is an amount quantity
#' @export
is_mass <- function(q) is(q, "MediaToolsMass")

#' @describeIn quantities check whether something is a molecular mass quantity
#' @export
is_molecular_mass <- function(q) is(q, "MediaToolsMolecularMass")

#' @describeIn quantities check whether something is a molarity quantity
#' @export
is_molarity <- function(q) is(q, "MediaToolsMolarity")

#' @describeIn quantities check whether something is a density quantity
#' @export
is_density <- function(q) is(q, "MediaToolsDensity")

#' @describeIn quantities check whether something is a volume quantity
#' @export
is_volume <- function(q) is(q, "MediaToolsVolume")

#' @describeIn quantities check whether something is a pressure quantity
#' @export
is_pressure <- function(q) is(q, "MediaToolsPressure")

#' @describeIn quantities check whether something is a solubility quantity
#' @export
is_solubility <- function(q) is(q, "MediaToolsSolubility")

#' @describeIn quantities check whether something is a temperature quantity
#' @export
is_temperature <- function(q) is(q, "MediaToolsTemperature")


# value return ======

#' Get quantity information
#' 
#' @details \code{get_qty_value}: get the value of a quantity in the desired unit. By default returns the quantity in the units it is in.
#' @name quantity_info
#' @inheritParams qty
#' @family quantities
#' @export
get_qty_value <- function(q, unit = get_qty_units(q)) UseMethod("get_qty_value", q)

#' @export
get_qty_value.numeric <- function(q, unit = get_qty_units(q)) stop("not a quantity: ", class(q)[1], call. = FALSE)

#' @export
get_qty_value.MediaToolsQuantity <- function(q, unit = get_qty_units(q)) {
  prefix <- get_unit_prefix(unit, get_base_unit(q))
  scaling <- get_metric_scale_factor(q, prefix)
  return(scaling * q@.Data)
}

#' @export
get_qty_value.MediaToolsPressure <- function(q, unit = get_qty_units(q)) {
  unit_conversion <- get_pressure_unit_conversion(unit)
  unit <- unit_conversion$unit
  x <- NextMethod()
  return(x/unit_conversion$conversion)
}

#' @export
get_qty_value.MediaToolsSolubility <- function(q, unit = get_qty_units(q)) {
  unit_conversion <- get_solubility_unit_conversion(unit)
  unit <- unit_conversion$unit
  x <- NextMethod()
  return(x/unit_conversion$conversion)
}

#' @export
get_qty_value.MediaToolsTemperature <- function(q, unit = get_qty_units(q)) {
  unit_conversion <- get_temperature_unit_conversion(unit)
  unit <- unit_conversion$unit
  x <- NextMethod()
  return(unit_conversion$conversion_back(x))
}

#' @details \code{get_qty_text}: get the value of the quantity in the desired unit as a text string with the unit appended
#' @rdname quantity_info
#' @param signif number of significant digits for printing the quantity
#' @export
get_qty_text <- function(q, unit = get_qty_units(q), signif = 5) {
  paste(base::signif(get_qty_value(q, unit), signif), unit)
}

# expand S4 methods ========================

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
# note: this is not yet supported in paged tables (i.e. knitted data frames display) because rmarkdown:::paged_table_type_sum does not use tibble::type_sum
#' @export
type_sum.MediaToolsQuantity <- function(x) {
  return(x@unit)
}

#' @export
as_factor.MediaToolsQuantity <- function(x) {
  return(forcats::as_factor(as.character(x)))
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

#' @details \code{get_qty_units}: get units from a quantity, list of quantities or data frame (returns NA for objects/columns that are not quantities)
#' @rdname quantity_info
#' @param q quantity or list of quantities
#' @export
get_qty_units <- function(q) {
  if (is_qty(q)) 
    return(q@unit)
  else if (is.list(q))
    return(purrr::map_chr(q, ~if(is_qty(.x)) { .x@unit } else { NA_character_ }))
  else
    return(NA_character_)
}

#' @details \code{get_qty_units_with_label} get units from a quantity, list of quantities or data frame, with a custom label in the format \code{label [units]}. Objects/columns that are not quantities simply return the label with out the [units] part.
#' @param label text label to use with the units - single value or vector of the same length as \code{q}. By default uses the names of \code{q}, which only works if \code{q} is a list or data frame.
#' @rdname quantity_info
#' @examples 
#' # labels with units
#' get_qty_units_with_label(qty(0.1, "mM"), "concentration")
#' 
#' # make labels with units for data frame columns
#' x <- data.frame(a = qty(1, "mg"), b = 2, c = qty(100, "mbar"))
#' get_qty_units_with_label(x, names(x)) 
#' @export
get_qty_units_with_label <- function(q, label = names(q)) {
  units <- get_qty_units(q)
  if (length(label) == 1 || length(label) == length(units)) {
    
  } else {
    sprintf("incompatible number of labels (%d) provided for units (%d)", length(label), length(units)) %>% 
      stop(call. = FALSE)
  }
  return(ifelse(is.na(units), label, sprintf("%s [%s]", label, units)))
}

# metric conversions ======================

#' Metric prefixes
#' 
#' These functions simplify converting between different metric prefixes.
#' 
#' @name metric
NULL

# convenience function to determine metric scaling factor
get_metric_scale_factor <- function(q, prefix) {
  metric_prefix <- get_mediatools_constant("metric_prefix")
  if (!is_qty(q)) stop("not a known type of quantity: ", class(q))
  if (! prefix %in% names(metric_prefix)) stop("not a known metric prefix: ", prefix)
  q_prefix <- get_prefix(q)
  if (q_prefix == prefix) return(1) # already the requested metric (for speed)
  
  # conversion
  scale_factor <- metric_prefix[[which(names(metric_prefix)==q_prefix)]]/ # complication required because of unity unit with "" name
    metric_prefix[[which(names(metric_prefix)==prefix)]]
  return(scale_factor)
}

#' @describeIn metric scale to a specific metrix prefix (from whatever the quantity is currently in)
#' @param q the \link{quantity} to scale
#' @param prefix a metric prefix (p, n, µ, m, k, M, etc.)
#' @family quantities
#' @export
scale_metric <- function (q, prefix = "") {
  scale_factor <- get_metric_scale_factor(q, prefix)
  q@.Data <- scale_factor * q@.Data
  q@unit <- paste0(prefix, get_base_unit(q))
  return(q)
}

#' @describeIn metric convert to best metric prefix (i.e. one that gives at least 1 significant digit before the decimal), 
#' if the quantity has a vector of values, scales to the best metric prefix for the median of all values
#' @export
best_metric <- function(q) {
  if (!is_qty(q)) stop("not a known type of quantity: ", class(q))
  prefix <- get_mediatools_constant("metric_prefix")
  if (length(q) == 0 || all(is.na(q) | is.infinite(q))) {
    ideal <- which(names(prefix) == "")
  } else {
    values <- as.numeric(base_metric(q)) %>% { .[!is.infinite(.)] }
    ideal <- max(1, which( median(abs(values), na.rm = TRUE)/prefix >= 1))
  }
  return(scale_metric(q, names(prefix)[ideal]))
}


#' @describeIn metric convert to base metric prefix of mediatools (i.e. to mol, L, etc.)
#' @export
base_metric <- function(q) {
  if (!is_qty(q)) stop("not a known type of quantity: ", class(q))
  if (get_prefix(q) == "") return(q) # already base metric (fater this way)
  else return(scale_metric(q, prefix = ""))
}

# Get the base unit of a quantiy
get_base_unit <- function(q) {
  return(new(class(q))@unit)
}

# Get the prefix of a quantity
get_prefix <- function(q) {
  return(get_unit_prefix(q@unit, get_base_unit(q)))
}
  
# Get the prefix from a unit
get_unit_prefix <- function(unit, base_unit) {
  if (! grepl(paste0(base_unit, "$"), unit)) {
    sprintf("not a valid unit for this quantity (base unit '%s'): %s", base_unit, unit) %>%
      stop(call. = FALSE) 
  }
  return(sub(paste0(base_unit, "$"), "", unit))
}

