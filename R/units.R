
#' Generate a quantity
qty <- function(x, unit) {
  if (!is.null(r <- tryCatch(concentration(x, unit), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(volume(x, unit), error = function(e){}))) return(r)
  if (!is.null(r <- tryCatch(amount(x, unit), error = function(e){}))) return(r)
  stop("Could not determine the appropriate quantity for unit ", unit)
}

#' Generate an amount
#' @export
amount <- function(x, unit) {
  units <- paste0(names(.si_prefix), "mol")
  if (! unit %in% units) stop("not a known amount unit: ", unit)
  new("Amount", x, unit = unit)
}

#' Generate a concentration
#' @export
concentration <- function(x, unit) {
  primary_units <- paste0(names(.si_prefix), "M")
  secondary_units <- paste0(names(.si_prefix), "mol/L")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known concentration unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  new("Molarity", x, unit = unit)
}

#' Generate a volume
#' @export
volume <- function(x, unit) {
  primary_units <- paste0(names(.si_prefix), "L")
  secondary_units <- paste0(names(.si_prefix), "l")
  if (! unit %in% c(primary_units, secondary_units)) stop("not a known volume unit: ", unit)
  
  if (unit %in% secondary_units)
    unit <- primary_units[secondary_units == unit]
  
  new("Volume", x, unit = unit)
}

#' SI conversions

.si_prefix <- c('p'=1e-12, 'n' = 1e-9, 'µ' = 1e-6, 'm' = 1e-3, 1, 
                'k' = 1e3, 'M' = 1e6, 'G' = 1e9)

#' Convert to different si unit
#' @param si an si prefix (p, n, µ, m, k, M, etc.)
#' @export
si <- function (q, si = "") {

  if (!inherits(q, "Quantity")) stop("not a known type of quantity: ", class(q))
  if (! si %in% names(.si_prefix)) stop("not a known si prefix: ", si)
  base_unit <- new(class(q))@unit
  if (! grepl(paste0(base_unit, "$"), q@unit)) stop("not a valid unit: ", q@unit) # this should never happen
  q_prefix <- sub(paste0(base_unit, "$"), "", q@unit)
  
  # conversion
  q@.Data <- (.si_prefix[[which(names(.si_prefix)==q_prefix)]]/   # complication required because of unity unit with "" name
                .si_prefix[[which(names(.si_prefix)==si)]]) * q@.Data
  q@unit <- paste0(si, base_unit)
  return(q)
}

#' Convert to base si unit
#' @export
base_si <- function(q) {
  si(q, "")
}

#' Convert to ideal si unit (i.e. one that gives at least 2 signif before the decimal)
#' Converts to that of the median for multiple values
#' @export
ideal_si <- function(q) {
  ideal <- max(1, which( median(base_si(q))/.si_prefix >= 1))
  si(q, names(.si_prefix)[ideal])
}

