# all the constants used by this package

.onLoad <- function(libname, pkgname) {
  
  bar_per_atm <- 1.01325
  
  # metric
  opts <- list(
    metric_prefix = 
      c(f = 1e-15, p = 1e-12, n = 1e-9, '\U00B5' = 1e-6, m = 1e-3, 1, 
        k = 1e3, M = 1e6, G = 1e9, T = 1e12),
    
    # pressure units
    bar_per_pa = 1e-5,
    bar_per_atm = bar_per_atm,
    bar_per_psi = 1/14.50377,
    bar_per_Torr = 1/760 * bar_per_atm,
    
    # temperature units
    celsius_kelvin_offset = -273.15,
    fahrenheit_celsius_offset = 32,
    fahrenheit_celsius_slope = 9/5,
    
    # physical constants
    R_in_L_bar_per_K_mol = 0.08314462 # ideal gas constant in the units used as base units by mediachemtools (L bar K-1 mol-1)
  )
  names(opts) <- paste0("mediachemtools_", names(opts))
  options(opts)
}

#' Constants
#' 
#' List and retrieve constants used in mediachemtools.
#' 
#' @name constants
NULL 
 
#' @describeIn constants get the value of a constant
#' @param name name of the constant
#' @export
get_mediachemtools_constant <- function(name) {
  value <- getOption(paste0("mediachemtools_", name))
  if (is.null(value)) stop("constant ", name, " is not specified")
  return(value)
}

#' @describeIn constants list all constants
#' @export
get_mediachemtools_constants <- function() {
  opts <- options() %>% {.[names(.) %>% stringr::str_detect("^mediachemtools_")]}
  data_frame(
    constant = names(opts) %>% stringr::str_replace("^mediachemtools_", ""),
    key = purrr::map(opts, names) %>% purrr::map( ~ if (is.null(.x)) { NA_character_ } else { .x }),
    value = purrr::map(opts, identity)
  ) %>% unnest(key, value)
}