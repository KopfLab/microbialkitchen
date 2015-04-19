context("Units")

test_that("Testing that units work and can be interconverted", {
  
  # constants
  expect_error(get_constant("bla"), "not specified")
  
  # concentration objects
  expect_error(concentration(1, "J"), "not a known concentration unit")
  expect_is(concentration(1, "mM"), "Molarity")
  expect_equal(concentration(1, "mM")@unit, "mM")
  expect_equal(concentration(1, "mmol/L")@unit, "mM")
  
  # metric conversion
  expect_error(scale_metric(1, "p"), "not a known type of quantity")
  expect_error(scale_metric(concentration(1, "mM"), "x"), "not a known metric prefix")
  expect_error({a <- concentration(1, "mM"); a@unit <- "J"; scale_metric(a, "m")}, "not a valid unit")
  expect_equal(scale_metric(concentration(1, "mM"), "n")@unit, "nM")
  expect_equal(scale_metric(concentration(1, "mM"), "n")@.Data, 1e6)
  expect_equal(scale_metric(concentration(1, "M"), "m")@.Data, 1e3)
  expect_equal(scale_metric(concentration(1, "µM"), "")@.Data, 1e-6)
  expect_equal(base_metric(concentration(1, "kM"))@.Data, 1e3)
  expect_equal(best_metric(concentration(0.2, "M"))@unit, "mM")
  expect_equal(best_metric(concentration(-5000, "nM"))@unit, "µM")
  expect_equal(best_metric(concentration(c(100, 1200, 1500), "pM"))@unit, "nM")
  
  # amout
  expect_error(amount(1, "J"), "not a known amount unit")
  expect_is(amount(1, "mol"), "Amount")
  expect_equal(amount(1, "nmol")@unit, "nmol")
  expect_equal(amount(1, "mole")@unit, "mol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = F)@unit, "nmol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = T)@unit, "µmol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = T)@.Data, 1)
  
  # mass
  expect_error(mass(1, "J"), "not a known mass unit")
  expect_is(mass(1, "mg"), "Mass")
  expect_equal(mass(1, "kg")@unit, "kg")
  expect_equal(mass(0.01, "g")@unit, "mg")
  expect_equal(mass(0.01, "g")@.Data, 10)

  # molecular weight
  expect_error(molecular_weight(1, "J"), "not a known molecular weight unit")
  expect_is(molecular_weight(1, "g/mol"), "MolecularWeight")
  expect_equal(molecular_weight(1257, "g/mol")@unit, "g/mol") # by default no automatic scaling!
  expect_equal(molecular_weight(1257, "g/mol")@.Data, 1257)
  
  # volume
  expect_error(volume(1, "mM"), "not a known volume unit")
  expect_is(volume(1, "mL"), "Volume")
  expect_equal(volume(1, "nL")@unit, "nL")
  
  # pressure
  expect_error(pressure(1, "J"), "not a known pressure unit")
  expect_is(pressure(1, "bar"), "Pressure")
  expect_equal(pressure(1, "mbar")@unit, "mbar")
  expect_equal(pressure(1, "atm")@.Data, 1.01325)
  expect_equal(pressure(0.1, "MPa")@.Data, 1)
  expect_equal(pressure(1, "kPa", scale_to_best_metric = FALSE)@.Data, get_constant("bar_per_pa"))
  expect_equal(pressure(1, "kPa", scale_to_best_metric = FALSE)@unit, "kbar")
  expect_equal(pressure(760, "Torr")@unit, "bar")
  expect_equal(pressure(760, "Torr")@.Data, 1.01325)
  expect_equal(pressure(760, "mTorr")@unit, "mbar")
  expect_equal(pressure(760, "mTorr")@.Data, 1.01325)
  
  # temperature
  expect_error(temperature(1, "J"), "not a known temperature unit")
  expect_is(temperature(0, "C"), "Temperature")
  expect_equal(temperature(100, "C")@unit, "K")
  expect_equal(temperature(100, "C")@.Data, 373.15)
  expect_equal(temperature(50, "F")@unit, "K")
  expect_equal(temperature(50, "F")@.Data, 283.15)
  
  # general quantity
  expect_error(qty(1, "kBla"), "Could not determine the appropriate quantity")
  expect_is(qty(1, "nM"), "Molarity")
  expect_is(qty(1, "L"), "Volume")
  expect_is(qty(1, "pmol"), "Amount")
  expect_equal(qty(1500, "pmol")@unit, "nmol")
  expect_equal(qty(1500, "pmol")@.Data, 1.5)
  expect_equal(qty(30, "C")@.Data, 303.15)
  expect_equal(qty(1250, "µg")@unit, "mg")
  expect_equal(qty(1250, "µg")@.Data, 1.25)
  expect_equal(qty(1250, "g/mol")@.Data, 1250)
})
  