context("Units")

test_that("Testing that units work and can be metric scaled", {
  
  # constants
  expect_error(get_mediatools_constant("bla"), "not specified")
  
  # concentration (molarity) objects
  expect_error(molarity(1, "J"), "not a known concentration")
  expect_is(molarity(1, "mM"), "MediaToolsMolarity")
  expect_equal(molarity(1, "mM") %>% get_qty_units(), "mM")
  expect_equal(molarity(1, "mmol/L") %>% get_qty_units(), "mM")
  
  # metric conversion
  expect_error(scale_metric(1, "p"), "not a known type of quantity")
  expect_error(scale_metric(molarity(1, "mM"), "x"), "not a known metric prefix")
  expect_error({a <- molarity(1, "mM"); a@unit <- "J"; scale_metric(a, "m")}, "not a valid unit")
  expect_equal(scale_metric(molarity(1, "mM"), "n") %>% get_qty_units(), "nM")
  expect_equal(scale_metric(molarity(1, "mM"), "n") %>% get_qty_value(), 1e6)
  expect_equal(scale_metric(molarity(1, "M"), "m") %>% get_qty_value(), 1e3)
  expect_equal(scale_metric(molarity(1, "µM"), "") %>% get_qty_value(), 1e-6)
  expect_error(base_metric(1), "not a known type of quantity")
  expect_equal(base_metric(molarity(1, "kM")) %>% get_qty_value(), 1e3)
  expect_equal(molarity(1, "kM") %>% get_qty_value("M"), 1e3)
  expect_equal(molarity(1, "kM") %>% get_qty_value("mM"), 1e6)
  expect_equal(best_metric(molarity(0.2, "M", scale_to_best_metric = FALSE)) %>% get_qty_units(), "mM")
  expect_equal(best_metric(molarity(-5000, "nM", scale_to_best_metric = FALSE)) %>% get_qty_units(), "µM")
  expect_equal(best_metric(molarity(c(100, 1200, 1500), "pM", scale_to_best_metric = FALSE)) %>% get_qty_units(), "nM")
  
  # density
  expect_error(density(1, "J"), "not a known concentration")
  expect_is(density(1, "g/L"), "MediaToolsDensity")
  expect_equal(density(1, "mg/L") %>% get_qty_units(), "mg/L")
  expect_equal(density(1, "ng/l") %>% get_qty_units(), "ng/L")
  expect_equal(density(0.1, "ng/L") %>% get_qty_units(), "pg/L")
  expect_equal(density(0.1, "ng/L") %>% get_qty_value(), 100)
  expect_equal(density(0.1, "ng/L") %>% get_qty_value("ng/L"), 0.1)
  
  # amout
  expect_error(amount(1, "J"), "not a known amount unit")
  expect_is(amount(1, "mol"), "MediaToolsAmount")
  expect_equal(amount(1, "nmol") %>% get_qty_units(), "nmol")
  expect_equal(amount(1, "mole") %>% get_qty_units(), "mol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = F) %>% get_qty_units(), "nmol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = T) %>% get_qty_units(), "µmol")
  expect_equal(amount(1000, "nmol", scale_to_best_metric = T) %>% get_qty_value(), 1)
  expect_equal(amount(1, "nmol") %>% get_qty_value("pmol"), 1000)
  
  # mass
  expect_error(mass(1, "J"), "not a known mass unit")
  expect_is(mass(1, "mg"), "MediaToolsMass")
  expect_equal(mass(1, "kg") %>% get_qty_units(), "kg")
  expect_equal(mass(0.01, "g") %>% get_qty_units(), "mg")
  expect_equal(mass(0.01, "g") %>% get_qty_value(), 10)
  expect_equal(mass(0.01, "g") %>% get_qty_value("g"), 0.01)

  # molecular mass
  expect_error(molecular_mass(1, "J"), "not a known molecular mass unit")
  expect_is(molecular_mass(1, "g/mol"), "MediaToolsMolecularMass")
  expect_equal(molecular_mass(1257, "g/mol") %>% get_qty_units(), "kg/mol") 
  expect_equal(molecular_mass(1257, "g/mol") %>% get_qty_value(), 1.257)
  expect_equal(molecular_mass(1257, "g/mol") %>% get_qty_value("g/mol"), 1257)
  expect_equal(molecular_mass(2, "kDa") %>% get_qty_units(), "kg/mol")
  expect_equal(molecular_mass(2, "kDa") %>% get_qty_value(), 2)
  
  # volume
  expect_error(volume(1, "mM"), "not a known volume unit")
  expect_is(volume(1, "mL"), "MediaToolsVolume")
  expect_equal(volume(1, "nL") %>% get_qty_units(), "nL")
  expect_equal(volume(1, "nL") %>% get_qty_value(), 1)
  expect_equal(volume(1, "nL") %>% get_qty_value("pL"), 1000)
  
  # pressure
  expect_error(pressure(1, "J"), "not a known pressure unit")
  expect_is(pressure(1, "bar"), "MediaToolsPressure")
  expect_equal(pressure(1, "mbar") %>% get_qty_units(), "mbar")
  expect_equal(pressure(1, "atm") %>% get_qty_value(), 1.01325)
  expect_equal(pressure(0.1, "MPa") %>% get_qty_value(), 1)
  expect_equal(pressure(1, "kPa", scale_to_best_metric = FALSE) %>% get_qty_value(), get_mediatools_constant("bar_per_pa"))
  expect_equal(pressure(1, "kPa", scale_to_best_metric = FALSE) %>% get_qty_units(), "kbar")
  expect_equal(pressure(760, "Torr") %>% get_qty_units(), "bar")
  expect_equal(pressure(760, "Torr") %>% get_qty_value(), 1.01325)
  expect_equal(pressure(760, "Torr") %>% get_qty_value("Torr"), 760)
  expect_equal(pressure(760, "Torr") %>% get_qty_value("atm"), 1)
  expect_equal(pressure(760, "mTorr") %>% get_qty_units(), "mbar")
  expect_equal(pressure(760, "mTorr") %>% get_qty_value(), 1.01325)
  expect_equal(pressure(760, "mTorr") %>% get_qty_value("mTorr"), 760)
  expect_equal(pressure(760, "mTorr") %>% get_qty_value("Torr"), 0.760)
  expect_equal(pressure(760, "mTorr") %>% get_qty_value("atm"), 0.001)
  
  # solubility
  expect_error(solubility(1, "J"), "not a known solubility unit")
  expect_is(solubility(1, "mM/bar"), "MediaToolsSolubility")
  expect_equal(solubility(1, "mM/bar") %>% get_qty_units(), "mM/bar")
  expect_equal(solubility(10, "mM/atm") %>% get_qty_value(), 10/get_mediatools_constant("bar_per_atm"))
  expect_equal(solubility(10, "mM/atm") %>% get_qty_value("mM/atm"), 10)
  expect_equal(solubility(10, "mM/atm") %>% get_qty_value("M/atm"), 0.01)
  
  # temperature
  expect_error(temperature(1, "J"), "not a known temperature unit")
  expect_is(temperature(0, "C"), "MediaToolsTemperature")
  expect_equal(temperature(100, "C") %>% get_qty_units(), "K")
  expect_equal(temperature(100, "C") %>% get_qty_value(), 373.15)
  expect_equal(temperature(100, "C") %>% get_qty_value("C"), 100)
  expect_equal(temperature(50, "F") %>% get_qty_units(), "K")
  expect_equal(temperature(50, "F") %>% get_qty_value(), 283.15)
  expect_equal(temperature(50, "F") %>% get_qty_value("F"), 50)
  expect_equal(temperature(50, "F") %>% get_qty_value("C"), 10)
  
  # general quantity
  expect_error(qty(1, "kBla"), "Could not determine the appropriate quantity")
  expect_true(is_molarity(qty(1, "nM")))
  expect_true(is_density(qty(1, "mg/L")))
  expect_true(is_volume(qty(1, "L")))
  expect_true(is_amount(qty(1, "pmol")))
  expect_true(is_pressure(qty(1, "kbar")))
  expect_true(is_solubility(qty(1, "nM/bar")))
  expect_true(is_temperature(qty(1, "K")))
  expect_equal(qty(1500, "pmol") %>% get_qty_units(), "nmol")
  expect_equal(qty(1500, "pmol", scale_to_best_metric = FALSE) %>% get_qty_units(), "pmol")
  expect_equal(qty(1500, "pmol") %>% get_qty_value(), 1.5)
  expect_equal(qty(30, "C") %>% get_qty_value(), 303.15)
  expect_equal(qty(1250, "µg") %>% get_qty_units(), "mg")
  expect_equal(qty(1250, "µg") %>% get_qty_value(), 1.25)
  expect_equal(qty(1250, "g/mol") %>% get_qty_value(), 1.250)
  expect_equal(qty(NA_real_, "mg") %>% get_qty_units(), "g")
  expect_equal(qty(numeric(0), "mg") %>% get_qty_units(), "g")
  expect_equal(qty(Inf, "mg") %>% get_qty_units(), "g")
  expect_equal(qty(-Inf, "mg") %>% get_qty_units(), "g")
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
  
  # get units with labels
  expect_error(1 %>% get_qty_units_with_label(), "incompatible number of labels")
  expect_equal(1 %>% get_qty_units_with_label("test"), "test")
  expect_equal(qty(1, "mg") %>% get_qty_units_with_label("test"), "test [mg]")
  expect_equal(
    list(a=qty(1, "mg"), b = 2) %>% get_qty_units_with_label("test2"),
    c(a = "test2 [mg]", b = "test2")
  )
  expect_equal(
    data.frame(a=qty(1, "mg"), b = 2) %>% get_qty_units_with_label("test3"),
    c(a = "test3 [mg]", b = "test3")
  )
  expect_equal(
    data.frame(a=qty(1, "mg"), b = 2) %>% get_qty_units_with_label(),
    c(a = "a [mg]", b = "b")
  )
  
  # units prefix
  expect_error(get_unit_prefix("mg", "L"), "not a valid unit for this quantity")
  expect_equal(get_unit_prefix("mg", "g"), "m")
  expect_equal(get_unit_prefix("g", "g"), "")
  expect_equal(get_unit_prefix("kM/bar", "M/bar"), "k")
  expect_equal(get_prefix(qty(1, "mg")), "m")
  
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
  