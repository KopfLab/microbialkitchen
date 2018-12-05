#' Calculate the concentration of an ideal gas
#' 
#' Calculate the gas phase equivalent concentration of an ideal gas at a specific temperature (i.e. mol of gas / L of volume).
#' @param pressure the pressure quantity of the gas
#' @param temperature the temperature quantity of the gas
#' @return molarity quantity
#' @family calculations
#' @export
calculate_ideal_gas_molarity <- function(pressure, temperature) {
  
  require_quantity(rlang::enquo(pressure), is_pressure)
  require_quantity(rlang::enquo(temperature), is_temperature)
  
  temperature.K <- get_qty_value(temperature, "K")
  pressure.bar <- get_qty_value(pressure, "bar")
  R_ideal <- get_mediatools_constant("R_in_L_bar_per_K_mol") 
  molarity.M <- pressure.bar / (R_ideal * temperature.K)
  
  return(molarity(molarity.M, "M"))
}

#' Calculate the amount of an ideal gas
#' 
#' Calculate the amount of an ideal gas at a specific pressure, temperature and volume
#' @inheritParams calculate_ideal_gas_molarity
#' @param volume the volume quantity of the gas
#' @return amount quantity
#' @family calculations
#' @export
calculate_ideal_gas_amount <- function(pressure, temperature, volume) {
  molarity <- calculate_ideal_gas_molarity(pressure, temperature)
  require_quantity(rlang::enquo(volume), is_volume)
  return(molarity * volume)
}

#' Calculate solubility of a gas
#' 
#' Calculate Henry's law solubility constant for a gas at a specific temperature. Henry's law constants are from: Sander, R. Compilation of Henry's law constants (version 4.0) for water as solvent. Atmos Chem Phys 15, 4399–4981 (2015). https://www.atmos-chem-phys.net/15/4399/2015/
#' 
#' @param name of the gas
#' @param temperature temperature quantity
#' @family calculations
#' @return solubility quantity
calculate_solubility <- function(gas, temperature) {
  
  # consider storing more centrally for easy modification/extension?
  constants <- 
    tibble::tribble(
      ~gas,   ~`dH/d(1/T)`, ~T0,     ~H0,       
      # name, slope         K,       M/bar = 100 * mol/m3/Pa 
      "CO2",  2400,         298.15,  3.3 * 10^-4 * 100
    )
  
  # safety checks
  if (missing(gas)) stop("gas is missing", call. = FALSE)
  gas_constants <- dplyr::filter(constants, gas == !!gas)
  if (nrow(gas_constants) == 0) 
    stop("no constants stored for gas ", gas, call. = FALSE)
  else if (nrow(gas_constants) > 1)
    stop("more than one set of constants for gas ", gas, call. = FALSE)
  require_quantity(rlang::enquo(temperature), is_temperature)
  
  # calculation
  temperature.K <- get_qty_value(temperature, "K")
  KH <- with(gas_constants, H0 * exp(`dH/d(1/T)` * (1/temperature.K - 1/T0) ))
  return(solubility(KH, "M/bar"))
}
