# all the constants used by this package

# metric
.metric_prefix <- c(f = 1e-15, p = 1e-12, n = 1e-9, 'µ' = 1e-6, m = 1e-3, 1, 
                    k = 1e3, M = 1e6, G = 1e9, T = 1e12)

# pressure units
.chemtools_bar_per_pa <- 1e-5
.chemtools_bar_per_atm <- 1.01325
.chemtools_bar_per_psi <- 1/14.50377
.chemtools_bar_per_Torr <- 1/760 * .chemtools_bar_per_atm

# temperature units
.chemtools_celsius_kelvin_offset <- -273.15
.chemtools_fahrenheit_celsius_offset <- 32
.chemtools_fahrenheit_celsius_slope <- 9/5

# physical constants
.chemtools_R_in_L_bar_per_K_mol <- 0.08314462 # ideal gas constant in the units used as base units by chemtools (L bar K-1 mol-1)

#' Constants
#' 
#' List and retrieve constants used in chemtools.
#' 
#' @name constants
NULL 
 
#' @describeIn constants get the value of a constant
#' @param name name of the constant
#' @export
get_constant <- function(name) {
  c_name <- paste0(".chemtools_", name)
  if (!exists(c_name, env = .GlobalEnv)) stop("constant ", name, " is not specified")
  return(get(c_name, env = .GlobalEnv))
}

#' @describeIn constants list all constants
#' @export
constants <- function() {
  vars <- ls(pattern = "\\.chemtools_.+", all.names = T, envir = .GlobalEnv)
  vals <- sapply(vars, get, envir = .GlobalEnv, USE.NAMES = F)
  data.frame(Constant = sub(".chemtools_", "", vars), Value = vals, stringsAsFactors = F)
}