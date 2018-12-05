# functions for carbonate chemistry calculations ====

#' Function to calculate DIC
#' 
#' Calculates the concentration of dissolved inorganic carbon based on pH and H2CO3* (aqueous CO2 + H2CO3) concentration or CO2 pressure.
#' @param pH the pH of the system
#' @param pCO2 the partial pressure of CO2 (a pressure quantity)
#' @param H2CO3* the concentration of H2CO3* (a molarity quantity), by default is calculated automatically from pCO2
#' @param solubility the solubility of CO2, by default is determined based on the temperature of the system
#' @param temperature the temperature of the system, used to calculate the solubility constant
#' @param pKa1 the acid dissociation constant for H2CO3*
#' @param pKa2 the acid dissociation constant for bicarbonate (HCO3-)
#' @return DIC concentration (a molarity quantity).
#' @family carbonate chemistry
#' @examples 
#' cc_calculate_DIC(pH = 7.0, pCO2 = qty(0.4, "mbar"))
#' cc_calculate_DIC(pH = 7.0, pCO2 = qty(0.4, "mbar"), temperature = qty(0, "C"))
#' @export
calculate_DIC <- function(
  pH, pCO2, 
  `H2CO3*` = solubility * pCO2, solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3) {
  
  # safety checks
  pCO2_quo <- rlang::enquo(pCO2)
  H2CO3_quo <- rlang::enquo(`H2CO3*`)
  if (missing(pH)) stop("pH is required", call. = FALSE)
  if (missing(`H2CO3*`) && missing(pCO2))  stop("either pCO2 or H2CO3* need to be specified", call. = FALSE)
  if (missing(`H2CO3*`)) require_quantity(pCO2_quo, is_pressure)
  require_quantity(H2CO3_quo, is_molarity)
  
  # calculation
  return(`H2CO3*` * (1 + 10^(pH - pKa1) + 10^(2*pH - pKa1 - pKa2)))
}

#' Calculate pH in open system (i.e. unlimited pCO2)
#' 
#' @inheritParams calculate_DIC
#' @param buffer [optional] total buffer concentration (a molarity quantity): a protonated buffer, make sure to add same amount to hard_ions IF a buffer salt is used (e.g. Na-buffer), and provide the appropriate pKa.
#' @param pKa buffer acid dissociation constant, required if \code{buffer} is provided
#' @param unbalanced_ions [optional] concentration of all unbalanced ions [units charge x a molarity quantity]. That means all charge multiplied cations from hard bases and included soft bases (carbonate and the specific buffer) e.g. mol/L Na that was added in the form of NaOH, NaHCO3 or as part of a Na-buffer salt. Also all charge multiplied (and -) anions from hard acids, e.g. -1 x mol/L Cl that was added as HCl or -2 x mol/L SO4 that was added as H2SO4. 
#' @return pH
#' @export
calculate_open_system_pH <- function(
  pCO2, 
  `H2CO3*` = solubility * pCO2, solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), pKa, unbalanced_ions = qty(0, "M")) {
  
  # safety checks
  pCO2_quo <- rlang::enquo(pCO2)
  H2CO3_quo <- rlang::enquo(`H2CO3*`)
  buffer_quo <- rlang::enquo(buffer)
  if (missing(`H2CO3*`) && missing(pCO2))  stop("either pCO2 or H2CO3* need to be specified", call. = FALSE)
  if (!missing(pCO2) && !missing(`H2CO3*`)) warning("H2CO3* provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  if (missing(`H2CO3*`)) require_quantity(pCO2_quo, is_pressure)
  require_quantity(H2CO3_quo, is_molarity)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(pKa))
    pKa <- 7 # place holder value, buffer is 0
  require_quantity(rlang::enquo(unbalanced_ions), is_molarity)
  
  # calculate
  buffer.M <- get_qty_value(buffer, unit = "M")
  `H2CO3*.M` <- get_qty_value(`H2CO3*`, unit = "M")
  unbalanced_ions.M <- get_qty_value(unbalanced_ions, unit = "M")
  result <- 
    dplyr::data_frame(`H2CO3*.M`, buffer.M, pKa, unbalanced_ions.M, pKa1, pKa2, pKw) %>% 
    dplyr::mutate(
      pH = mapply(
        function(`H2CO3*.M`, buffer.M, pKa, unbalanced_ions.M, pKa1, pKa2, pKw) {
          calc_root <- function(pH) {
            unbalanced_ions.M - 
              calculate_open_system_unbalanced_ions_formula(
                pH, `H2CO3*.M`, pKa1, pKa2, pKw, buffer.M, pKa)
          }
          return(uniroot(calc_root, c(0, 14))$root)
        },
        `H2CO3*.M`, buffer.M, pKa, unbalanced_ions.M, pKa1, pKa2, pKw)
    )
  return(result$pH)
}

#' Calculate unbalanced ions of an open system
#' 
#' Function to calculate the net unbalanced ions (e.g. Na+ added as NaOH or NaHCO3 minus Cl- added HCl) of an open system that is carbonate buffered and has an optional additional weak acid buffer.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_open_system_pH
#' @return unbalanced ion concentration (a molarity quantity), positive = excess cations, negative = excess anions
#' @export
calculate_open_system_unbalanced_ions <- function(
  pH, pCO2, `H2CO3*` = solubility * pCO2, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), pKa) {
  
  # safety checks
  if (missing(pH)) stop("pH is required", call. = FALSE)
  pCO2_quo <- rlang::enquo(pCO2)
  H2CO3_quo <- rlang::enquo(`H2CO3*`)
  buffer_quo <- rlang::enquo(buffer)
  if (missing(`H2CO3*`) && missing(pCO2))  stop("either pCO2 or H2CO3* need to be specified", call. = FALSE)
  if (!missing(pCO2) && !missing(`H2CO3*`)) warning("H2CO3* provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  if (missing(`H2CO3*`)) require_quantity(pCO2_quo, is_pressure)
  require_quantity(H2CO3_quo, is_molarity)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(pKa))
    pKa <- 7 # place holder value, buffer is 0
  
  # calculation
  buffer.M <- get_qty_value(buffer, unit = "M")
  `H2CO3*.M` <- get_qty_value(`H2CO3*`, unit = "M")
  unbalanced_ions <- 
    calculate_open_system_unbalanced_ions_formula(
      pH, `H2CO3*.M`, pKa1, pKa2, pKw, buffer.M, pKa)
  return(molarity(unbalanced_ions, "M"))
}

# helper function for calculating unbalanced ions
calculate_open_system_unbalanced_ions_formula <- function(pH, `H2CO3*.M`, pKa1, pKa2, pKw, buffer.M, pKa) {
  -10^(-pH) +
    1/(1 + 10^(pKa - pH)) * buffer.M +
    (2 * 10^(2*pH - pKa1 - pKa2) + 10^(pH - pKa1)) * `H2CO3*.M` +
    10^(pH - pKw)
}

#' Total inorganic carbon in a closed system
#' 
#' Calculates total inorganic carbon carbon in a closed system at equlibrium with its headspace.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_closed_system_pH
#' @return total amount of inorganic carbon (an amount quantity)
#' @export
calculate_closed_system_TIC <- function(pH, pCO2, V_gas, V_liquid, temperature = qty(25, "C")) {
  # safety checks
  if (missing(pH)) stop("pH is required", call. = FALSE)
  require_quantity(rlang::enquo(pCO2), is_pressure)
  require_quantity(rlang::enquo(V_gas), is_volume)
  require_quantity(rlang::enquo(V_liquid), is_volume)
  require_quantity(rlang::enquo(temperature), is_temperature)
  
  # gas phase CO2
  calculate_ideal_gas_amount(pressure = pCO2, temperature = temperature, volume = V_gas) +
    # liquid phase CO2
    calculate_DIC(pH = pH, pCO2 = pCO2, temperature = temperature) * V_liquid
}

#' CO2 pressure in a closed system
#' 
#' Calculates the partial pressure of CO2 in the headspace of a closed system at equilibrium between gas and liquid.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_closed_system_pH
#' @return pCO2 (a pressure quantity)
#' @export
calculate_closed_system_pCO2 <- function(
  pH, TIC, V_gas, V_liquid, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3) {
  
  # safety checks
  if (missing(pH)) stop("pH is required", call. = FALSE)
  require_quantity(rlang::enquo(TIC), is_amount)
  require_quantity(rlang::enquo(V_gas), is_volume)
  require_quantity(rlang::enquo(V_liquid), is_volume)
  require_quantity(rlang::enquo(temperature), is_temperature)
  
  temperature.K <- get_qty_value(temperature, "K")
  TIC.mol <- get_qty_value(TIC, "mol")
  V_gas.L <- get_qty_value(V_gas, "L")
  V_liquid.L <- get_qty_value(V_liquid, "L")
  solubility.M_bar <- get_qty_value(solubility, "M/bar")
  R_ideal <- get_mediatools_constant("R_in_L_bar_per_K_mol") 
  
  pCO2.bar <- TIC.mol / 
    ( V_gas.L / (R_ideal * temperature.K) + 
        V_liquid.L * solubility.M_bar * (1 + 10^(pH - pKa1) + 10^(2*pH - pKa1 - pKa2)) )
  return(pressure(pCO2.bar, "bar"))
}

#' Calculate pH i a closed system
#' 
#' @param TIC total inorganic carbon in system (an amount quantity)
#' @param V_liquid volume of liquid (a volume quantity)
#' @param V_gas volume of the gas/headsapce (a volume quantity)
#' @inheritParams calculate_open_system_pH
#' @return pH
calculate_closed_system_pH <- function(
  TIC, V_liquid, V_gas, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), pKa, unbalanced_ions = qty(0, "M")){
  
  # safety checks
  require_quantity(rlang::enquo(TIC), is_amount)
  require_quantity(rlang::enquo(V_gas), is_volume)
  require_quantity(rlang::enquo(V_liquid), is_volume)
  require_quantity(rlang::enquo(temperature), is_temperature)
  buffer_quo <- rlang::enquo(buffer)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(pKa))
    pKa <- 7 # place holder value, buffer is 0
  require_quantity(rlang::enquo(unbalanced_ions), is_molarity)
  
  # calculate
  TIC.mol <- get_qty_value(TIC, unit = "mol")
  buffer.M <- get_qty_value(buffer, unit = "M")
  V_gas.L <- get_qty_value(V_gas, "L")
  V_liquid.L <- get_qty_value(V_liquid, "L")
  solubility.M_bar <- get_qty_value(solubility, "M/bar")
  temperature.K <- get_qty_value(temperature, "K")
  unbalanced_ions.M <- get_qty_value(unbalanced_ions, unit = "M")
  result <- 
    dplyr::data_frame(TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, pKa, temperature.K, solubility.M_bar, unbalanced_ions.M) %>% 
    dplyr::mutate(
      pH = mapply(
        function(TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, pKa, temperature.K, solubility.M_bar, unbalanced_ions.M) {
          calc_root <- function(pH) {
            unbalanced_ions.M - 
              calculate_closed_system_unbalanced_ions_formula(
                pH, TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, pKa, temperature.K, solubility.M_bar)
          }
          return(uniroot(calc_root, c(0, 14))$root)
        },
        TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, pKa, temperature.K, solubility.M_bar, unbalanced_ions.M)
    )
  return(result$pH)
}

#' Calculate unbalanced ions of a closed system
#' 
#' Function to calculate the unbalanced ions (e.g. Na+ added as NaOH or NaHCO3 minus Cl- added HCl) of a closed system that is carbonate buffered and has an optional additional buffer.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_closed_system_pH
#' @return unbalanced ion concentration (a molarity quantity), positive = excess cations, negative = excess anions
#' @export
calculate_closed_system_unbalanced_ions <- function(
  pH, TIC, V_liquid, V_gas, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), pKa) {
  
  # safety checks
  if (missing(pH)) stop("pH is required", call. = FALSE)
  require_quantity(rlang::enquo(TIC), is_amount)
  require_quantity(rlang::enquo(V_gas), is_volume)
  require_quantity(rlang::enquo(V_liquid), is_volume)
  require_quantity(rlang::enquo(temperature), is_temperature)
  buffer_quo <- rlang::enquo(buffer)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(pKa))
    pKa <- 7 # place holder value, buffer is 0
  
  # calculation
  TIC.mol <- get_qty_value(TIC, unit = "mol")
  buffer.M <- get_qty_value(buffer, unit = "M")
  V_gas.L <- get_qty_value(V_gas, "L")
  V_liquid.L <- get_qty_value(V_liquid, "L")
  solubility.M_bar <- get_qty_value(solubility, "M/bar")
  temperature.K <- get_qty_value(temperature, "K")
  unbalanced_ions <- 
    calculate_closed_system_unbalanced_ions_formula(
      pH, TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, pKa, temperature.K, solubility.M_bar)
  return(molarity(unbalanced_ions, "M"))
}

# helper function for calculating unbalanced ions
calculate_closed_system_unbalanced_ions_formula <- function(pH, TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, pKa, temperature.K, solubility.M_bar) {
  
  R_ideal <- get_mediatools_constant("R_in_L_bar_per_K_mol") 
  -10^(-pH) + 
    1/(1 + 10^(pKa - pH)) * buffer.M +
    (
      (10^(pH-pKa1) + 2*10^(2*pH-pKa1-pKa2)) / 
        ( V_gas.L / (solubility.M_bar * R_ideal * temperature.K) + (1 + 10^(pH-pKa1) + 10^(2*pH-pKa1-pKa2)) * V_liquid.L)
    ) * TIC.mol +
    10^(pH-pKw)
}


