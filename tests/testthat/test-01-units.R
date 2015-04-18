context("Units")

test_that("Testing that units work and can be interconverted", {
  
  # concentration objects
  expect_error(concentration(1, "J"), "not a known concentration unit")
  expect_is(concentration(1, "mM"), "Molarity")
  expect_equal(concentration(1, "mM")@unit, "mM")
  expect_equal(concentration(1, "mmol/L")@unit, "mM")
  
  # si conversion
  expect_error(si(1, "p"), "not a known type of quantity")
  expect_error(si(concentration(1, "mM"), "x"), "not a known si prefix")
  expect_error({a <- concentration(1, "mM"); a@unit <- "J"; si(a, "m")}, "not a valid unit")
  expect_equal(si(concentration(1, "mM"), "n")@unit, "nM")
  expect_equal(si(concentration(1, "mM"), "n")@.Data, 1e6)
  expect_equal(si(concentration(1, "M"), "m")@.Data, 1e3)
  expect_equal(si(concentration(1, "µM"), "")@.Data, 1e-6)
  expect_equal(base_si(concentration(1, "kM"))@.Data, 1e3)
  expect_equal(ideal_si(concentration(0.2, "M"))@unit, "mM")
  expect_equal(ideal_si(concentration(c(100, 1200, 1500), "pM"))@unit, "nM")
  
  # amout
  expect_error(amount(1, "J"), "not a known amount unit")
  expect_is(amount(1, "mol"), "Amount")
  expect_equal(amount(1, "nmol")@unit, "nmol")
  
  # volume
  expect_error(volume(1, "mM"), "not a known volume unit")
  expect_is(volume(1, "mL"), "Volume")
  expect_equal(volume(1, "nL")@unit, "nL")
  
  # general quanity
  expect_error(qty(1, "kBla"), "Could not determine the appropriate quantity")
  expect_is(qty(1, "nM"), "Molarity")
  expect_is(qty(1, "L"), "Volume")
  expect_is(qty(1, "pmol"), "Amount")
})
  