context("Calculations")

test_that("Testing that carbonate chemistry speciation calculations are working", {
  
  # DIC =======
  expect_error(calculate_DIC(), "pH is required")
  expect_error(calculate_DIC(4.0), "either pCO2 or H2CO3*")
  expect_error(calculate_DIC(4.0, pCO2 = 1), "not a pressure")
  expect_error(calculate_DIC(4.0, `H2CO3*` = 5), "not a molarity")
  expect_error(calculate_DIC(4.0, pCO2 = qty(100, "mbar"), temperature = 1), "not a temperature")
  expect_error(calculate_DIC(4.0, pCO2 = qty(100, "mbar"), solubility = 1), "solubility \\* pCO2.* not a molarity")
  expect_warning(calculate_DIC(4.0, pCO2 = qty(100, "mbar"), `H2CO3*` = qty(1, "mM")), "pCO2 .* will be ignored")
  
  # FIXME: test proper result of calculation
  
  # Carbonic acid ======
  expect_error(calculate_carbonic_acid(), "pH is required")
  expect_error(calculate_carbonic_acid(4.0), "either pCO2 or DIC")
  expect_error(calculate_carbonic_acid(4.0, pCO2 = 1), "not a pressure")
  expect_error(calculate_carbonic_acid(4.0, DIC = 5), "not a molarity")
  expect_error(calculate_carbonic_acid(4.0, pCO2 = qty(100, "mbar"), temperature = 1), "not a temperature")
  expect_error(calculate_carbonic_acid(4.0, pCO2 = qty(100, "mbar"), solubility = 1), "solubility \\* pCO2.* not a molarity")
  expect_warning(calculate_carbonic_acid(4.0, pCO2 = qty(100, "mbar"), DIC = qty(1, "mM")), "pCO2 .* will be ignored")
  
  # FIXME: test proper result of calculation
  
  # Bicarbonate =====
  expect_error(calculate_bicarbonate(), "pH is required")
  expect_error(calculate_bicarbonate(4.0), "either pCO2 or DIC")
  expect_error(calculate_bicarbonate(4.0, pCO2 = 1), "not a pressure")
  expect_error(calculate_bicarbonate(4.0, DIC = 5), "not a molarity")
  expect_error(calculate_bicarbonate(4.0, pCO2 = qty(100, "mbar"), temperature = 1), "not a temperature")
  expect_error(calculate_bicarbonate(4.0, pCO2 = qty(100, "mbar"), solubility = 1), "solubility \\* pCO2.* not a molarity")
  expect_warning(calculate_bicarbonate(4.0, pCO2 = qty(100, "mbar"), DIC = qty(1, "mM")), "pCO2 .* will be ignored")
  
  # FIXME: test proper result of calculation
  
  # Carbonate =====
  expect_error(calculate_carbonate(), "pH is required")
  expect_error(calculate_carbonate(4.0), "either pCO2 or DIC")
  expect_error(calculate_carbonate(4.0, pCO2 = 1), "not a pressure")
  expect_error(calculate_carbonate(4.0, DIC = 5), "not a molarity")
  expect_error(calculate_carbonate(4.0, pCO2 = qty(100, "mbar"), temperature = 1), "not a temperature")
  expect_error(calculate_carbonate(4.0, pCO2 = qty(100, "mbar"), solubility = 1), "solubility \\* pCO2.* not a molarity")
  expect_warning(calculate_carbonate(4.0, pCO2 = qty(100, "mbar"), DIC = qty(1, "mM")), "pCO2 .* will be ignored")
  
  # FIXME: test proper result of calculation
  
  # Test combined calculation
  expect_equal(
    calculate_DIC(c(4.0, 7.0, 10.0), pCO2 = qty(100, "mbar")),
    calculate_carbonic_acid(c(4.0, 7.0, 10.0), pCO2 = qty(100, "mbar")) + 
      calculate_bicarbonate(c(4.0, 7.0, 10.0), pCO2 = qty(100, "mbar")) + 
      calculate_carbonate(c(4.0, 7.0, 10.0), pCO2 = qty(100, "mbar"))
  )
  
})

test_that("Testing that carbonate chemistry open system calculations are working", {
  
  # open system unbalanced ions =======
  expect_error(calculate_open_system_alkalinity(), "pH is required")
  expect_error(calculate_open_system_alkalinity(7), "either pCO2 or H2CO3*")
  expect_error(calculate_open_system_alkalinity(7, pCO2 = 1), "not a pressure")
  expect_error(calculate_open_system_alkalinity(7, `H2CO3*` = 5), "not a molarity")
  expect_error(calculate_open_system_alkalinity(7, pCO2 = qty(100, "mbar"), temperature = 1), "not a temperature")
  expect_error(calculate_open_system_alkalinity(7, pCO2 = qty(100, "mbar"), solubility = 1), "solubility \\* pCO2.* not a molarity")
  expect_error(calculate_open_system_alkalinity(7, pCO2 = qty(100, "mbar"), buffer = 1), "not a molarity")
  expect_error(calculate_open_system_alkalinity(7, pCO2 = qty(100, "mbar"), buffer = qty(1, "mM")), "must provide a pKa")
  expect_warning(calculate_open_system_alkalinity(7, pCO2 = qty(100, "mbar"), `H2CO3*` = qty(1, "mM")), "pCO2 .* ignored")
  
  # open system pH =========
  expect_error(calculate_open_system_pH(), "either pCO2 or H2CO3*")
  expect_error(calculate_open_system_pH(pCO2 = 1), "not a pressure")
  expect_error(calculate_open_system_pH(`H2CO3*` = 5), "not a molarity")
  expect_error(calculate_open_system_pH(pCO2 = qty(100, "mbar"), temperature = 1), "not a temperature")
  expect_error(calculate_open_system_pH(pCO2 = qty(100, "mbar"), solubility = 1), "solubility \\* pCO2.* not a molarity")
  expect_error(calculate_open_system_pH(pCO2 = qty(100, "mbar"), buffer = 1), "not a molarity")
  expect_error(calculate_open_system_pH(pCO2 = qty(100, "mbar"), buffer = qty(1, "mM")), "must provide a pKa")
  expect_error(calculate_open_system_pH(pCO2 = qty(100, "mbar"), alkalinity = 1), "not a molarity")
  expect_warning(calculate_open_system_pH(pCO2 = qty(100, "mbar"), `H2CO3*` = qty(1, "mM")), "pCO2 .* ignored")
  
  # FIXME: test proper result of calculation
  
  # check that open system unbalanced ions are consistent ========
  expect_equal(
    calculate_open_system_pH(qty(c(0.4, 10, 100, 500), "mbar"), alkalinity = qty(1, "mM")) %>% 
      calculate_open_system_alkalinity(qty(c(0.4, 10, 100, 500), "mbar")) %>% 
      get_qty_value("mM") %>% signif(4),
    c(1, 1, 1, 1)
  )
  expect_equal(
    calculate_open_system_pH(qty(c(0.4, 10, 100, 500), "mbar"), alkalinity = qty(-1, "mM")) %>% 
      calculate_open_system_alkalinity(qty(c(0.4, 10, 100, 500), "mbar")) %>% 
      get_qty_value("mM") %>% signif(4),
    c(-1, -1, -1, -1)
  )
  
  # calculate total inorganic carbon in closed system =======
  expect_error(calculate_closed_system_TIC(), "pH is required")
  expect_error(calculate_closed_system_TIC(7), "pressure is missing")
  expect_error(calculate_closed_system_TIC(7, pCO2 = 1), "not a pressure")
  expect_error(calculate_closed_system_TIC(7, pCO2 = qty(100, "mbar")), "volume is missing")
  expect_error(calculate_closed_system_TIC(7, pCO2 = qty(100, "mbar"), V_gas = 1), "not a volume")
  expect_error(calculate_closed_system_TIC(7, pCO2 = qty(100, "mbar"), V_gas = qty(1, "mL")), "volume is missing")
  expect_error(calculate_closed_system_TIC(7, pCO2 = qty(100, "mbar"), V_gas = qty(1, "mL"), V_liquid = 1), "not a volume")
  expect_error(calculate_closed_system_TIC(7, pCO2 = qty(100, "mbar"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), temperature = 1), "not a temperature")
  
  # FIXME: test proper result of calculation ========
  
  # calculate pCO2 in closed system
  expect_error(calculate_closed_system_pCO2(), "pH is required")
  expect_error(calculate_closed_system_pCO2(7), "amount is missing")
  expect_error(calculate_closed_system_pCO2(7, TIC = 1), "not a amount")
  expect_error(calculate_closed_system_pCO2(7, TIC = qty(100, "nmol")), "volume is missing")
  expect_error(calculate_closed_system_pCO2(7, TIC = qty(100, "nmol"), V_gas = 1), "not a volume")
  expect_error(calculate_closed_system_pCO2(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL")), "volume is missing")
  expect_error(calculate_closed_system_pCO2(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = 1), "not a volume")
  expect_error(calculate_closed_system_pCO2(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), temperature = 1), "not a temperature")
  
  # FIXME: test proper result of calculation
  
  # check that TIC and pCO2 back conversion is consisten
  pCO2 <- qty(100, "mbar")
  expect_true(
    {
      TIC <- calculate_closed_system_TIC(7.0, pCO2, qty(10, "ml"), qty(50, "ml"));
      (calculate_closed_system_pCO2(7.0, TIC,  qty(10, "ml"), qty(50, "ml")) - pCO2) < qty(1, "fbar")
    }
  )
  
})

test_that("Testing that carbonate chemistry closed system calculations are working", {
  
  # closed system unbalanced ions ======
  expect_error(calculate_closed_system_alkalinity(), "pH is required")
  expect_error(calculate_closed_system_alkalinity(7), "amount is missing")
  expect_error(calculate_closed_system_alkalinity(7, TIC = 1), "not a amount")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol")), "volume is missing")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol"), V_gas = 1), "not a volume")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL")), "volume is missing")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = 1), "not a volume")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), temperature = 1), "not a temperature")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), buffer = 1), "not a molarity")
  expect_error(calculate_closed_system_alkalinity(7, TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), buffer = qty(1, "mM")), "must provide a pKa")
  
  # FIXME: test proper result of calculation
  
  # closed system pH ========
  expect_error(calculate_closed_system_pH(), "amount is missing")
  expect_error(calculate_closed_system_pH(TIC = 1), "not a amount")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol")), "volume is missing")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = 1), "not a volume")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = qty(1, "mL")), "volume is missing")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = 1), "not a volume")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), temperature = 1), "not a temperature")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), buffer = 1), "not a molarity")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), buffer = qty(1, "mM")), "must provide a pKa")
  expect_error(calculate_closed_system_pH(TIC = qty(100, "nmol"), V_gas = qty(1, "mL"), V_liquid = qty(1, "mL"), alkalinity = 1), "not a molarity")
  
  # FIXME: test proper result of calculation
  
  # check that closed system unbalanced ions are consistent ========
  expect_equal(
    calculate_closed_system_pH(TIC = qty(c(0.1, 1, 5, 10), "mmol"), qty(10, "mL"), qty(100, "mL"), alkalinity = qty(1, "mM")) %>% 
      calculate_closed_system_alkalinity(TIC = qty(c(0.1, 1, 5, 10), "mmol"), qty(10, "mL"), qty(100, "mL")) %>% 
      get_qty_value("mM") %>% signif(4),
    c(1, 1, 1, 1)
  )
  expect_equal(
    calculate_closed_system_pH(TIC = qty(c(0.1, 1, 5, 10), "mmol"), qty(10, "mL"), qty(100, "mL"), alkalinity = qty(-1, "mM")) %>% 
      calculate_closed_system_alkalinity(TIC = qty(c(0.1, 1, 5, 10), "mmol"), qty(10, "mL"), qty(100, "mL")) %>% 
      get_qty_value("mM") %>% signif(4),
    c(-1, -1, -1, -1)
  )
  
})