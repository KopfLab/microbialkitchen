context("Units")

test_that("Testing that units work and can be metric scaled", {
  
  # constants
  expect_error(cht_get_constant("bla"), "not specified")
  
  # concentration (molarity) objects
  expect_error(cht_molarity(1, "J"), "not a known concentration")
  expect_is(cht_molarity(1, "mM"), "Molarity")
  expect_equal(cht_molarity(1, "mM")@unit, "mM")
  expect_equal(cht_molarity(1, "mmol/L")@unit, "mM")
  
  # metric conversion
  expect_error(cht_scale_metric(1, "p"), "not a known type of quantity")
  expect_error(cht_scale_metric(cht_molarity(1, "mM"), "x"), "not a known metric prefix")
  expect_error({a <- cht_molarity(1, "mM"); a@unit <- "J"; cht_scale_metric(a, "m")}, "not a valid unit")
  expect_equal(cht_scale_metric(cht_molarity(1, "mM"), "n")@unit, "nM")
  expect_equal(cht_scale_metric(cht_molarity(1, "mM"), "n")@.Data, 1e6)
  expect_equal(cht_scale_metric(cht_molarity(1, "M"), "m")@.Data, 1e3)
  expect_equal(cht_scale_metric(cht_molarity(1, "µM"), "")@.Data, 1e-6)
  expect_equal(cht_base_metric(cht_molarity(1, "kM"))@.Data, 1e3)
  expect_equal(cht_best_metric(cht_molarity(0.2, "M"))@unit, "mM")
  expect_equal(cht_best_metric(cht_molarity(-5000, "nM"))@unit, "µM")
  expect_equal(cht_best_metric(cht_molarity(c(100, 1200, 1500), "pM"))@unit, "nM")
  
  # density
  expect_error(cht_density(1, "J"), "not a known concentration")
  expect_is(cht_density(1, "g/L"), "Density")
  expect_equal(cht_density(1, "mg/L")@unit, "mg/L")
  expect_equal(cht_density(1, "ng/l")@unit, "ng/L")
  
  # amout
  expect_error(cht_amount(1, "J"), "not a known amount unit")
  expect_is(cht_amount(1, "mol"), "Amount")
  expect_equal(cht_amount(1, "nmol")@unit, "nmol")
  expect_equal(cht_amount(1, "mole")@unit, "mol")
  expect_equal(cht_amount(1000, "nmol", scale_to_best_metric = F)@unit, "nmol")
  expect_equal(cht_amount(1000, "nmol", scale_to_best_metric = T)@unit, "µmol")
  expect_equal(cht_amount(1000, "nmol", scale_to_best_metric = T)@.Data, 1)
  
  # mass
  expect_error(cht_mass(1, "J"), "not a known mass unit")
  expect_is(cht_mass(1, "mg"), "Mass")
  expect_equal(cht_mass(1, "kg")@unit, "kg")
  expect_equal(cht_mass(0.01, "g")@unit, "mg")
  expect_equal(cht_mass(0.01, "g")@.Data, 10)

  # molecular weight
  expect_error(cht_molecular_weight(1, "J"), "not a known molecular weight unit")
  expect_is(cht_molecular_weight(1, "g/mol"), "MolecularWeight")
  expect_equal(cht_molecular_weight(1257, "g/mol")@unit, "g/mol") # by default no automatic scaling!
  expect_equal(cht_molecular_weight(1257, "g/mol")@.Data, 1257)
  
  # volume
  expect_error(cht_volume(1, "mM"), "not a known volume unit")
  expect_is(cht_volume(1, "mL"), "Volume")
  expect_equal(cht_volume(1, "nL")@unit, "nL")
  
  # pressure
  expect_error(cht_pressure(1, "J"), "not a known pressure unit")
  expect_is(cht_pressure(1, "bar"), "Pressure")
  expect_equal(cht_pressure(1, "mbar")@unit, "mbar")
  expect_equal(cht_pressure(1, "atm")@.Data, 1.01325)
  expect_equal(cht_pressure(0.1, "MPa")@.Data, 1)
  expect_equal(cht_pressure(1, "kPa", scale_to_best_metric = FALSE)@.Data, cht_get_constant("bar_per_pa"))
  expect_equal(cht_pressure(1, "kPa", scale_to_best_metric = FALSE)@unit, "kbar")
  expect_equal(cht_pressure(760, "Torr")@unit, "bar")
  expect_equal(cht_pressure(760, "Torr")@.Data, 1.01325)
  expect_equal(cht_pressure(760, "mTorr")@unit, "mbar")
  expect_equal(cht_pressure(760, "mTorr")@.Data, 1.01325)
  
  # temperature
  expect_error(cht_temperature(1, "J"), "not a known temperature unit")
  expect_is(cht_temperature(0, "C"), "Temperature")
  expect_equal(cht_temperature(100, "C")@unit, "K")
  expect_equal(cht_temperature(100, "C")@.Data, 373.15)
  expect_equal(cht_temperature(50, "F")@unit, "K")
  expect_equal(cht_temperature(50, "F")@.Data, 283.15)
  
  # general quantity
  expect_error(qty(1, "kBla"), "Could not determine the appropriate quantity")
  expect_is(qty(1, "nM"), "Molarity")
  expect_is(qty(1, "L"), "Volume")
  expect_is(qty(1, "pmol"), "Amount")
  expect_equal(qty(1500, "pmol")@unit, "nmol")
  expect_equal(qty(1500, "pmol", scale_to_best_metric = FALSE)@unit, "pmol")
  expect_equal(qty(1500, "pmol")@.Data, 1.5)
  expect_equal(qty(30, "C")@.Data, 303.15)
  expect_equal(qty(1250, "µg")@unit, "mg")
  expect_equal(qty(1250, "µg")@.Data, 1.25)
  expect_equal(qty(1250, "g/mol")@.Data, 1250)
  expect_equal(qty(NA_real_, "mg")@unit, "g")
  expect_equal(qty(numeric(0), "mg")@unit, "g")
  
  # get units
  expect_equal(1 %>% get_qty_units(), NA_character_)
  expect_equal(qty(1, "mg") %>% get_qty_units(), "mg")
  expect_equal(
    list(a=qty(1, "mg"), b = 2) %>% get_qty_units(),
    c(a = "mg", b = NA_character_)
  )
  expect_equal(
    data.frame(a=qty(1, "mg"), b = 2) %>% get_qty_units(),
    c(a = "mg", b = NA_character_)
  )
})
  