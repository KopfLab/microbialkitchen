# all the constants used by this package

.onLoad <- function(libname, pkgname) {
  
  bar_per_atm <- 1.01325
  
  # constants
  opts <- list(
    # base units
    base_units = c(
      "amount" = "mol",
      "mass" = "g",
      "molecular_mass" = "g/mol",
      "molarity" = "M",
      "density" = "g/L",
      "volume" = "L",
      "pressure" = "bar",
      "solubility" = "M/bar",
      "temperature" = "K"
    ),
    
    # metric prefixes
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
    R_in_L_bar_per_K_mol = 0.08314462 # ideal gas constant in the units used as base units by microbialkitchen (L bar K-1 mol-1)
  )
  names(opts) <- paste0("microbialkitchen_", names(opts))
  options(opts)
}

#' Constants
#' 
#' List and retrieve constants used in microbialkitchen.
#' 
#' @name constants
NULL 
 
#' @describeIn constants get the value of a constant
#' @param name name of the constant
#' @export
get_microbialkitchen_constant <- function(name) {
  value <- getOption(paste0("microbialkitchen_", name))
  if (is.null(value)) stop("constant ", name, " is not specified")
  return(value)
}

#' @describeIn constants list all constants
#' @export
get_microbialkitchen_constants <- function() {
  opts <- options() %>% {.[names(.) %>% stringr::str_detect("^microbialkitchen_")]}
  data_frame(
    constant = names(opts) %>% stringr::str_replace("^microbialkitchen_", ""),
    key = purrr::map(opts, names) %>% purrr::map( ~ if (is.null(.x)) { NA_character_ } else { .x }),
    value = purrr::map(opts, identity)
  ) %>% unnest(key, value)
}

# get metric prefixes
get_base_units <- function() {
  get_microbialkitchen_constant("base_units")
}

# get metric prefixes
get_metric_prefixes <- function() {
  get_microbialkitchen_constant("metric_prefix")
}
