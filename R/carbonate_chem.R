# general functions ====

#' Speciation functions
#' 
#' Calculate dissolved inorganic carbon (DIC) and individual carbonate species in an open or closes system at equilibrium.
#' \itemize{
#'  \item{carbonic acid* = H2CO3* (aqueous CO2 + H2CO3)}
#'  \item{bicarbonate = HCO3(-)}
#'  \item{carbonate = CO3(2-)}  
#' }
#' 
#' @name speciation
#' @family carbonate chemistry
NULL

#' @describeIn speciation calculates the concentration of dissolved inorganic carbon based on pH and either pCO2 or H2CO3*. Returns DIC as a molarity quantity.
#' @param pH the pH of the system
#' @param pCO2 the partial pressure of CO2 (a pressure quantity)
#' @param H2CO3* the concentration of H2CO3* (a molarity quantity), by default is calculated automatically from pCO2
#' @param solubility the solubility of CO2, by default is determined based on the temperature of the system
#' @param temperature the temperature of the system, used to calculate the solubility constant
#' @param pKa1 the acid dissociation constant for H2CO3*
#' @param pKa2 the acid dissociation constant for bicarbonate (HCO3-)
#' @examples 
#' calculate_DIC(pH = 7.0, pCO2 = qty(0.4, "mbar"))
#' calculate_DIC(pH = 7.0, pCO2 = qty(0.4, "mbar"), temperature = qty(0, "C"))
#' calculate_DIC(pH = 7.0, `H2CO3*` = qty(1, "mM"))
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
  
  # warning
  if (!missing(pCO2) && !missing(`H2CO3*`)) warning("H2CO3* provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  
  # calculation
  return(`H2CO3*` * (1 + 10^(pH - pKa1) + 10^(2*pH - pKa1 - pKa2)))
}

#' @describeIn speciation calculates the concentration of carbonic acid (H2CO3*) based on pH and either pCO2 or DIC. Returns carbonic acid as a molarity quantity.
#' @param DIC dissolved inorganic carbon (a molarity quantity)
#' @export
calculate_carbonic_acid <- function(
  pH, pCO2, DIC, solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3) {
  
  # safety checks
  pCO2_quo <- rlang::enquo(pCO2)
  DIC_quo <- rlang::enquo(DIC)
  if (missing(pH)) stop("pH is required", call. = FALSE)
  if (missing(DIC) && missing(pCO2))  stop("either pCO2 or DIC need to be specified", call. = FALSE)
  if (missing(DIC)) { 
    require_quantity(pCO2_quo, is_pressure)
    DIC <- calculate_DIC(pH, pCO2, solubility = solubility, temperature = temperature, pKa1 = pKa1, pKa2 = pKa2)
  } else {
    require_quantity(DIC_quo, is_molarity)
    if (!missing(pCO2)) warning("DIC provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  }

  # calculation
  return(DIC / (1 + 10^(pH-pKa1) + 10^(2*pH-pKa1-pKa2)))
}

#' @describeIn speciation calculates the concentration of bicarbonate based on pH and either pCO2 or DIC. Returns bicarbonate concentration as a molarity quantity.
#' @export
calculate_bicarbonate <- function(
  pH, pCO2, DIC, solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3) {
  
  # safety checks
  pCO2_quo <- rlang::enquo(pCO2)
  DIC_quo <- rlang::enquo(DIC)
  if (missing(pH)) stop("pH is required", call. = FALSE)
  if (missing(DIC) && missing(pCO2))  stop("either pCO2 or DIC need to be specified", call. = FALSE)
  if (missing(DIC)) { 
    require_quantity(pCO2_quo, is_pressure)
    DIC <- calculate_DIC(pH, pCO2, solubility = solubility, temperature = temperature, pKa1 = pKa1, pKa2 = pKa2)
  } else {
    require_quantity(DIC_quo, is_molarity)
    if (!missing(pCO2)) warning("DIC provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  }
  
  # calculation
  return(DIC / (10^(pKa1-pH) + 1 + 10^(pH-pKa2)))
}

#' @describeIn speciation calculates the concentration of carbonate based on pH and either pCO2 or DIC. Returns carbonate concentration as a molarity quantity.
#' @export
calculate_carbonate <- function(
  pH, pCO2, DIC, solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3) {
  
  # safety checks
  pCO2_quo <- rlang::enquo(pCO2)
  DIC_quo <- rlang::enquo(DIC)
  if (missing(pH)) stop("pH is required", call. = FALSE)
  if (missing(DIC) && missing(pCO2))  stop("either pCO2 or DIC need to be specified", call. = FALSE)
  if (missing(DIC)) { 
    require_quantity(pCO2_quo, is_pressure)
    DIC <- calculate_DIC(pH, pCO2, solubility = solubility, temperature = temperature, pKa1 = pKa1, pKa2 = pKa2)
  } else {
    require_quantity(DIC_quo, is_molarity)
    if (!missing(pCO2)) warning("DIC provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  }
  
  # calculation
  return(DIC / (10^(pKa1+pKa2-2*pH) + 10^(pKa2-pH) + 1))
}

# open system ==============

#' Open system calculations
#' 
#' Functions to calculate pH and alkalinity in an open, carbonate-buffered system at equilibrium (i.e. with an unlimited CO2 reservoir) with an optional additional weak acid/base buffer. This is for simplified aqueous geochemical systems commonly encountered in culturing applications. All default constants are based on freshwater and there is no implicit salinity correction included (stability constants can be set manually). For more complex systems, please use a full aquatic chemistry suite instead.
#' 
#' @name open_system
#' @family carbonate chemistry
NULL

#' @describeIn open_system calculate the pH of an open system. Returns pH.
#' @inheritParams calculate_DIC
#' @param pKw water dissociation constant
#' @param buffer [optional] total concentration of the pH buffer (a molarity quantity). Assumes that this is a protonated weak acid with the provided \code{buffer_pKa}. For weak base buffers, provide a negative molarity quantity instead and the appropriate \code{buffer_pKa}. If the buffer is a salt (e.g. Na-Buffer or Buffer-Cl), make sure to add the appropriate molarity to the \code{alkalinity} to account for the added ions (positive for hard cations, negative for hard anions). By default no additional buffer is added.
#' @param buffer_pKa buffer acid dissociation constant, required if \code{buffer} is provided
#' @param alkalinity [optional] charge-weighed NET concentration of all conservative ions [units charge x a molarity quantity] (explicitly conservative alkalinity). Conservative ions are those that do NOT get affected by changes in pH in the pH range of interest (i.e. do not form any acids or bases or have pKas far outside the pH range of interest). E.g. mol/L Na that was added in the form of NaOH, NaHCO3 or as part of a Na-buffer salt; -1 x mol/L Cl that was added as HCl or -2 x mol/L SO4 that was added as H2SO4. Ions from salts that are comprised exclusively of conservative ions (e.g. NaCl, MgSO4) do not need to be included because they cancel out. By default the alkalinity of the system is 0 M.
#' @examples 
#' calculate_open_system_pH(pCO2 = qty(100, "mbar"))
#' @export
calculate_open_system_pH <- function(
  pCO2, 
  `H2CO3*` = solubility * pCO2, solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), buffer_pKa, alkalinity = qty(0, "M")) {
  
  # safety checks
  pCO2_quo <- rlang::enquo(pCO2)
  H2CO3_quo <- rlang::enquo(`H2CO3*`)
  buffer_quo <- rlang::enquo(buffer)
  if (missing(`H2CO3*`) && missing(pCO2))  stop("either pCO2 or H2CO3* need to be specified", call. = FALSE)
  if (!missing(pCO2) && !missing(`H2CO3*`)) warning("H2CO3* provided, pCO2 parameter will be ignored", call. = FALSE, immediate. = TRUE)
  if (missing(`H2CO3*`)) require_quantity(pCO2_quo, is_pressure)
  require_quantity(H2CO3_quo, is_molarity)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(buffer_pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(buffer_pKa))
    buffer_pKa <- 7 # place holder value, buffer is 0
  require_quantity(rlang::enquo(alkalinity), is_molarity)
  
  # calculate
  buffer.M <- get_qty_value(buffer, unit = "M")
  `H2CO3*.M` <- get_qty_value(`H2CO3*`, unit = "M")
  alkalinity.M <- get_qty_value(alkalinity, unit = "M")
  result <- 
    dplyr::data_frame(`H2CO3*.M`, buffer.M, buffer_pKa, alkalinity.M, pKa1, pKa2, pKw) %>% 
    dplyr::mutate(
      pH = mapply(
        function(`H2CO3*.M`, buffer.M, buffer_pKa, alkalinity.M, pKa1, pKa2, pKw) {
          calc_root <- function(pH) {
            alkalinity.M - 
              calculate_open_system_alkalinity_formula(
                pH, `H2CO3*.M`, pKa1, pKa2, pKw, buffer.M, buffer_pKa)
          }
          return(stats::uniroot(calc_root, c(0, 14))$root)
        },
        `H2CO3*.M`, buffer.M, buffer_pKa, alkalinity.M, pKa1, pKa2, pKw)
    )
  return(result$pH)
}

#' @describeIn open_system calculates the alkalinity of an open system that is carbonate buffered and has an optional additional weak acid/base buffer. Returns the alkalinity as a molarity quantity. positive = excess cations, negative = excess anions.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_open_system_pH
#' @export
calculate_open_system_alkalinity <- function(
  pH, pCO2, `H2CO3*` = solubility * pCO2, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), buffer_pKa) {
  
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
  if (!missing(buffer) && missing(buffer_pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(buffer_pKa))
    buffer_pKa <- 7 # place holder value, buffer is 0
  
  # calculation
  buffer.M <- get_qty_value(buffer, unit = "M")
  `H2CO3*.M` <- get_qty_value(`H2CO3*`, unit = "M")
  alkalinity <- 
    calculate_open_system_alkalinity_formula(
      pH, `H2CO3*.M`, pKa1, pKa2, pKw, buffer.M, buffer_pKa)
  return(molarity(alkalinity, "M"))
}

# helper function for calculating unbalanced ions
calculate_open_system_alkalinity_formula <- function(pH, `H2CO3*.M`, pKa1, pKa2, pKw, buffer.M, buffer_pKa) {
  -10^(-pH) +
    1/(1 + 10^(buffer_pKa - pH)) * buffer.M +
    (2 * 10^(2*pH - pKa1 - pKa2) + 10^(pH - pKa1)) * `H2CO3*.M` +
    10^(pH - pKw)
}

# closed system ==============

#' Closed system calculations
#' 
#' Functions to calculate pH, alkalinity, total inorganic carbon and pCO2 in a closed, carbonate-buffered system at equilibrium (i.e. finite liquid and gas/headspace volume at equilibrium) with an optional additional weak acid/base buffer. This is for simplified aqueous geochemical systems commonly encountered in culturing applications. All default constants are based on freshwater and there is no implicit salinity correction included (stability constants can be set manually). For more complex systems, please use a full aquatic chemistry suite instead.
#' 
#' @name closed_system
#' @family carbonate chemistry
NULL

#' @describeIn closed_system calculate the pH of a closed system. Returns pH.
#' @param TIC total inorganic carbon in the system (an amount quantity)
#' @param V_liquid volume of liquid (a volume quantity)
#' @param V_gas volume of the gas/headsapce (a volume quantity)
#' @inheritParams calculate_open_system_pH
#' @export
calculate_closed_system_pH <- function(
  TIC, V_liquid, V_gas, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), buffer_pKa, alkalinity = qty(0, "M")){
  
  # safety checks
  require_quantity(rlang::enquo(TIC), is_amount)
  require_quantity(rlang::enquo(V_gas), is_volume)
  require_quantity(rlang::enquo(V_liquid), is_volume)
  require_quantity(rlang::enquo(temperature), is_temperature)
  buffer_quo <- rlang::enquo(buffer)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(buffer_pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(buffer_pKa))
    buffer_pKa <- 7 # place holder value, buffer is 0
  require_quantity(rlang::enquo(alkalinity), is_molarity)
  
  # calculate
  TIC.mol <- get_qty_value(TIC, unit = "mol")
  buffer.M <- get_qty_value(buffer, unit = "M")
  V_gas.L <- get_qty_value(V_gas, "L")
  V_liquid.L <- get_qty_value(V_liquid, "L")
  solubility.M_bar <- get_qty_value(solubility, "M/bar")
  temperature.K <- get_qty_value(temperature, "K")
  alkalinity.M <- get_qty_value(alkalinity, unit = "M")
  result <- 
    dplyr::data_frame(TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, buffer_pKa, temperature.K, solubility.M_bar, alkalinity.M) %>% 
    dplyr::mutate(
      pH = mapply(
        function(TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, buffer_pKa, temperature.K, solubility.M_bar, alkalinity.M) {
          calc_root <- function(pH) {
            alkalinity.M - 
              calculate_closed_system_alkalinity_formula(
                pH, TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, buffer_pKa, temperature.K, solubility.M_bar)
          }
          return(stats::uniroot(calc_root, c(0, 14))$root)
        },
        TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, buffer_pKa, temperature.K, solubility.M_bar, alkalinity.M)
    )
  return(result$pH)
}

#' @describeIn closed_system calculates the alkalinity of a closed system that is carbonate buffered and has an optional additional weak acid/base buffer. Returns the alkalinity as a molarity quantity. positive = excess cations, negative = excess anions.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_closed_system_pH
#' @export
calculate_closed_system_alkalinity <- function(
  pH, TIC, V_liquid, V_gas, 
  solubility = calculate_solubility("CO2", temperature), temperature = qty(25, "C"),
  pKa1 = 6.3, pKa2 = 10.3, pKw = 14,
  buffer = qty(0, "M"), buffer_pKa) {
  
  # safety checks
  if (missing(pH)) stop("pH is required", call. = FALSE)
  require_quantity(rlang::enquo(TIC), is_amount)
  require_quantity(rlang::enquo(V_gas), is_volume)
  require_quantity(rlang::enquo(V_liquid), is_volume)
  require_quantity(rlang::enquo(temperature), is_temperature)
  buffer_quo <- rlang::enquo(buffer)
  require_quantity(buffer_quo, is_molarity)
  if (!missing(buffer) && missing(buffer_pKa))
    stop("must provide a pKa if buffer is provided", call. = FALSE)
  else if (missing(buffer_pKa))
    buffer_pKa <- 7 # place holder value, buffer is 0
  
  # calculation
  TIC.mol <- get_qty_value(TIC, unit = "mol")
  buffer.M <- get_qty_value(buffer, unit = "M")
  V_gas.L <- get_qty_value(V_gas, "L")
  V_liquid.L <- get_qty_value(V_liquid, "L")
  solubility.M_bar <- get_qty_value(solubility, "M/bar")
  temperature.K <- get_qty_value(temperature, "K")
  alkalinity <- 
    calculate_closed_system_alkalinity_formula(
      pH, TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, buffer_pKa, temperature.K, solubility.M_bar)
  return(molarity(alkalinity, "M"))
}

# helper function for calculating unbalanced ions
calculate_closed_system_alkalinity_formula <- function(pH, TIC.mol, V_liquid.L, V_gas.L, pKa1, pKa2, pKw, buffer.M, buffer_pKa, temperature.K, solubility.M_bar) {
  
  R_ideal <- get_mediachemtools_constant("R_in_L_bar_per_K_mol") 
  -10^(-pH) + 
    1/(1 + 10^(buffer_pKa - pH)) * buffer.M +
    (
      (10^(pH-pKa1) + 2*10^(2*pH - pKa1 - pKa2)) / 
        ( V_gas.L / (solubility.M_bar * R_ideal * temperature.K) + (1 + 10^(pH - pKa1) + 10^(2*pH - pKa1 - pKa2)) * V_liquid.L)
    ) * TIC.mol +
    10^(pH-pKw)
}

#' @describeIn closed_system calculates total inorganic carbon (TIC) in a closed system at equilibrium. Returns TIC as an amount quantity.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_closed_system_pH
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

#' @describeIn closed_system calculates the partial pressure of CO2 in the headspace (gas phase) of a closed system at equilibrium. Returns a pressure quantity.
#' @inheritParams calculate_DIC
#' @inheritParams calculate_closed_system_pH
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
  R_ideal <- get_mediachemtools_constant("R_in_L_bar_per_K_mol") 
  
  pCO2.bar <- TIC.mol / 
    ( V_gas.L / (R_ideal * temperature.K) + 
        V_liquid.L * solubility.M_bar * (1 + 10^(pH - pKa1) + 10^(2*pH - pKa1 - pKa2)) )
  return(pressure(pCO2.bar, "bar"))
}