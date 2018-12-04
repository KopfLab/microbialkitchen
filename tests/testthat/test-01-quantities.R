context("Units")

test_that("Testing that units work and can be metric scaled", {
  
  # constants
  expect_error(get_mediatools_constant("bla"), "not specified")
  
  # concentration (molarity) objects
  expect_error(molarity(1, "J"), "not a known concentration")
  expect_is(molarity(1, "mM"), "MediaToolsMolarity")
  expect_equal(molarity(1, "mM")@unit, "mM")
  expect_equal(molarity(1, "mmol/L")@unit, "mM")
  
  # metric conversion
  expect_error(scale_metric(1, "p"), "not a known type of quantity")
  expect_error(scale_metric(molarity(1, "mM"), "x"), "not a known metric prefix")
  expect_error({a <- molarity(1, "mM"); a@unit <- "J"; scale_metric(a, "m")}, "not a valid unit")
  expect_equal(scale_metric(molarity(1, "mM"), "n")@unit, "nM")
  expect_equal(scale_metric(molarity(1, "mM"), "n")@.Data, 1e6)
  expect_equal(scale_metric(molarity(1, "M"), "m")@.Data, 1e3)
  expect_equal(scale_metric(molarity(1, "µM"), "")@.Data, 1e-6)
  expect_equal(base_metric(molarity(1, "kM"))@.Data, 1e3)
  expect_equal(best_metric(molarity(0.2, "M"))@unit, "mM")
  expect_equal(best_metric(molarity(-5000, "nM"))@unit, "µM")
  expect_equal(best_metric(molarity(c(100, 1200, 1500), "pM"))@unit, "nM")
  
  # density
  expect_error(density(1, "J"), "not a known concentration")
  expect_is(density(1, "g/L"), "MediaToolsDensity")
  expect_equal(density(1, "mg/L")@unit, "mg/L")
  expect_equal(density(1, "ng/l")@unit, "ng/L")
  
  # amout
  expect_error(amount(1, "J"), "not a known amount unit")
  expect_is(amount(1, "mol"), "MediaToolsAmount")
  expect_equal(amount(1, "nmol")@unit, "nmol")
  expect_equal(amount(1, "mole")@unit, "mol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = F)@unit, "nmol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = T)@unit, "µmol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = T)@.Data, 1)
  
  # mass
  expect_error(mass(1, "J"), "not a known mass unit")
  expect_is(mass(1, "mg"), "MediaToolsMass")
  expect_equal(mass(1, "kg")@unit, "kg")
  expect_equal(mass(0.01, "g")@unit, "mg")
  expect_equal(mass(0.01, "g")@.Data, 10)

  # molecular mass
  expect_error(molecular_mass(1, "J"), "not a known molecular mass unit")
  expect_is(molecular_mass(1, "g/mol"), "MediaToolsMolecularMass")
  expect_equal(molecular_mass(1257, "g/mol")@unit, "kg/mol") 
  expect_equal(molecular_mass(1257, "g/mol")@.Data, 1.257)
  expect_equal(molecular_mass(2, "kDa")@unit, "kg/mol")
  expect_equal(molecular_mass(2, "kDa")@.Data, 2)
  
  # volume
  expect_error(volume(1, "mM"), "not a known volume unit")
  expect_is(volume(1, "mL"), "MediaToolsVolume")
  expect_equal(volume(1, "nL")@unit, "nL")
  
  # pressure
  expect_error(pressure(1, "J"), "not a known pressure unit")
  expect_is(pressure(1, "bar"), "MediaToolsPressure")
  expect_equal(pressure(1, "mbar")@unit, "mbar")
  expect_equal(pressure(1, "atm")@.Data, 1.01325)
  expect_equal(pressure(0.1, "MPa")@.Data, 1)
  expect_equal(pressure(1, "kPa", scale_to_best_metric = FALSE)@.Data, get_mediatools_constant("bar_per_pa"))
  expect_equal(pressure(1, "kPa", scale_to_best_metric = FALSE)@unit, "kbar")
  expect_equal(pressure(760, "Torr")@unit, "bar")
  expect_equal(pressure(760, "Torr")@.Data, 1.01325)
  expect_equal(pressure(760, "mTorr")@unit, "mbar")
  expect_equal(pressure(760, "mTorr")@.Data, 1.01325)
  
  # solubility
  expect_error(solubility(1, "J"), "not a known solubility unit")
  expect_is(solubility(1, "mM/bar"), "MediaToolsSolubility")
  expect_equal(solubility(1, "mM/bar")@unit, "mM/bar")
  expect_equal(solubility(10, "mM/atm")@.Data, 10/get_mediatools_constant("bar_per_atm"))
  
  # temperature
  expect_error(temperature(1, "J"), "not a known temperature unit")
  expect_is(temperature(0, "C"), "MediaToolsTemperature")
  expect_equal(temperature(100, "C")@unit, "K")
  expect_equal(temperature(100, "C")@.Data, 373.15)
  expect_equal(temperature(50, "F")@unit, "K")
  expect_equal(temperature(50, "F")@.Data, 283.15)
  
  # general quantity
  expect_error(qty(1, "kBla"), "Could not determine the appropriate quantity")
  expect_true(is_molarity(qty(1, "nM")))
  expect_true(is_density(qty(1, "mg/L")))
  expect_true(is_volume(qty(1, "L")))
  expect_true(is_amount(qty(1, "pmol")))
  expect_true(is_pressure(qty(1, "kbar")))
  expect_true(is_solubility(qty(1, "nM/bar")))
  expect_true(is_temperature(qty(1, "K")))
  expect_equal(qty(1500, "pmol")@unit, "nmol")
  expect_equal(qty(1500, "pmol", scale_to_best_metric = FALSE)@unit, "pmol")
  expect_equal(qty(1500, "pmol")@.Data, 1.5)
  expect_equal(qty(30, "C")@.Data, 303.15)
  expect_equal(qty(1250, "µg")@unit, "mg")
  expect_equal(qty(1250, "µg")@.Data, 1.25)
  expect_equal(qty(1250, "g/mol")@.Data, 1.250)
  expect_equal(qty(NA_real_, "mg")@unit, "g")
  expect_equal(qty(numeric(0), "mg")@unit, "g")
  expect_equal(qty(Inf, "mg")@unit, "g")
  expect_equal(qty(-Inf, "mg")@unit, "g")
  expect_true(is_qty(qty(1, "mg")))
  expect_false(is_qty(1))
  
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
  
  # empty / infinite / null vectors (all scale to base unit)
  expect_equal(qty(NA, "mg") %>% get_qty_units(), "g")
  expect_equal(qty(NA, "mg") %>% as.numeric(), NA_real_)
  expect_equal(qty(Inf, "mg") %>% get_qty_units(), "g")
  expect_equal(qty(Inf, "mg") %>% as.numeric(), Inf)
  expect_equal(qty(-Inf, "mg") %>% get_qty_units(), "g")
  expect_equal(qty(-Inf, "mg") %>% as.numeric(), -Inf)
  expect_equal(qty(numeric(0), "mg") %>% get_qty_units(), "g")
  expect_equal(qty(numeric(0), "mg") %>% as.numeric(), numeric(0))
  
  # empty / infinite /null vectors with other number
  expect_equal(qty(c(1, NA, -Inf, Inf), "mg") %>% get_qty_units(), "mg")
  expect_equal(qty(c(1, NA, -Inf, Inf), "mg") %>% as.numeric(), c(1, NA, -Inf, Inf))
})
  