context("Calculations")

test_that("Testing that ideal gas calculations are working", {
  
  # ideal gas molarity
  expect_error(calculate_ideal_gas_molarity(), "pressure is missing")
  expect_error(calculate_ideal_gas_molarity(1), "not a pressure")
  expect_error(calculate_ideal_gas_molarity(qty(1, "mbar")), "temperature is missing")
  expect_error(calculate_ideal_gas_molarity(qty(1, "mbar"), 1), "not a temperature")
  expect_equal(
    calculate_ideal_gas_molarity(qty(100, "mbar"), qty(0, "C")) %>% base_metric() %>% as.numeric(),
    0.1 / (273.15 * get_microbialkitchen_constant("R_in_L_bar_per_K_mol"))
  )
  
  # ideal gas amount
  expect_error(calculate_ideal_gas_amount(qty(1, "mbar"), qty(0, "C")), "volume is missing")
  expect_error(calculate_ideal_gas_amount(qty(1, "mbar"), qty(0, "C"), 1), "not a volume")
  expect_equal(
    calculate_ideal_gas_amount(qty(100, "mbar"), qty(0, "C"), qty(5, "mL")) %>% base_metric() %>% as.numeric(),
    0.005 * 0.1 / (273.15 * get_microbialkitchen_constant("R_in_L_bar_per_K_mol"))
  )
  
  # solubility
  expect_error(calculate_solubility(), "gas is missing")
  expect_error(calculate_solubility("x"), "no constants")
  expect_error(calculate_solubility("CO2"), "temperature is missing")
  expect_true(is_gas_solubility(s <- calculate_solubility("CO2", qty(25, "C"))))
  expect_equal(get_qty_units(s), "mM/bar")
  expect_equal(as.numeric(s), 33)
  
})
  
