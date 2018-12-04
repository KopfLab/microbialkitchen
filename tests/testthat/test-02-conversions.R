context("Conversions")

test_that("Testing that standard vector operations are working", {
  
  # subsetting
  expect_equal(qty(1:5, "g")[2:3], qty(2:3, "g"))
  expect_equal(qty(1:5, "g")[-5], qty(1:4, "g"))
  expect_equal(qty(1:5, "g")[10], qty(NA_real_, "g"))
  expect_equal(qty(1:5, "g")[-c(1:5)], qty(numeric(0), "g"))
  
  # combining
  expect_equal(c_qty(qty(5, "g"), qty(c(10, 20), "mg")), qty(c(5000, 10, 20), "mg"))
  expect_equal(c(qty(5, "g"), qty(c(10, 20), "mg")), qty(c(5000, 10, 20), "mg"))
  expect_equal(c(qty(NA, "g"), qty(c(10, 20), "mg")), qty(c(NA, 10, 20), "mg"))
  expect_equal(c(qty(numeric(0), "g"), qty(c(10, 20), "mg")), qty(c(10, 20), "mg"))
})

test_that("Testing that arithemtic calculations of units are working", {
  
  # comparisons
  expect_error(qty(2, "C") > qty(1, "g"), "comparison is not implemented for these quantities")
  expect_error(qty(2, "C") >= qty(1, "g"), "comparison is not implemented for these quantities")
  expect_error(qty(2, "C") < qty(1, "g"), "comparison is not implemented for these quantities")
  expect_error(qty(2, "C") <= qty(1, "g"), "comparison is not implemented for these quantities")
  expect_error(qty(2, "C") == qty(1, "g"), "comparison is not implemented for these quantities")
  expect_error(qty(2, "C") != qty(1, "g"), "comparison is not implemented for these quantities")
  expect_true(qty(100, "mg") < qty(1, "g"))
  expect_true(qty(100, "mg") <= qty(0.1, "g"))
  expect_true(qty(100, "mg") > qty(10000, "µg"))
  expect_true(qty(10, "mg") >= qty(10000, "µg"))
  expect_true(qty(10, "mg") == qty(10000, "µg"))
  expect_false(qty(10, "mg") != qty(10000, "µg"))
  
  # addition / subtraction
  expect_error(qty(2, "C") + 1, "addition is not implemented for these quantities")
  expect_error(1 + qty(2, "C"), "addition is not implemented for these quantities")
  expect_error(qty(2, "C") + qty(1, "g"), "addition is not implemented for these quantities")
  expect_error(qty(2, "C") - 1, "subtraction is not implemented for these quantities")
  expect_error(1 - qty(2, "C"), "subtraction is not implemented for these quantities")
  expect_error(qty(2, "C") - qty(1, "g"), "subtraction is not implemented for these quantities")
  expect_equal({q <- qty(1, "mg") + qty(5, "µg") - qty(0.0003, "g"); q@unit}, "µg")
  expect_equal(round(q@.Data), 705) # there's a rounding problem wiht machine error otherwise
  
  # division
  expect_error(qty(2, "C") / qty(1, "g"), "division is not implemented for these quantities")
  expect_error(1000 / qty(1, "g"), "division is not implemented for these quantities")
  expect_equal(qty(1, "g") / 1000, qty(1, "mg")) # auto-converts units
  expect_equal(qty(1, "g") / qty(1, "mg"), 1000) # should give plain number
  expect_equal(qty(5, "mM") / qty(1, "M"), 0.005) # should give plain number
  
  # multiplication
  expect_error(qty(1, "g") * qty(1, "g"), "multiplication is not implemented for these quantities")
  expect_equal(qty(5, "g") * 100, qty(500, "g"))
  expect_equal(100 * qty(5, "g"), qty(500, "g"))
  
  # amount / volume = concentration
  expect_equal(qty(5, "µmol") / qty(1, "ml"), qty(5, "mM"))
  expect_equal(qty(5, "pmol") / qty(10, "nL"), qty(500, "µM"))
  # amount / molarity = volume
  expect_equal(qty(5, "nmol") / qty(10, "mM"), qty(500, "nL"))
  # volume * molarity = amount
  expect_equal(qty(1, "ml") * qty(10, "mM"), qty(10, "µmol"))
  expect_equal(qty(10, "mM") * qty(1, "ml"), qty(10, "µmol"))
  # chain: volume * molarity / volume = amount
  expect_equal(qty(1, "ml") * qty(10, "mM") / qty(1, "L"), qty(10, "µM"))
  expect_equal(qty(1, "ml") * qty(0.5, "mM") / qty(100, "mM"), qty(5, "µL"))
  
  # mass / MW = amount
  expect_equal(qty(5, "mg") / qty(100, "g/mol"), qty(50, "µmol"))
  # mass / amount = MW
  expect_equal(qty(2, "mg") / qty(1, "µmol"), qty(2000, "g/mol"))
  # amount * MW = mass
  expect_equal(qty(10, "nmol") * qty(50, "g/mol"), qty(500, "ng"))
  expect_equal(qty(50, "g/mol") * qty(10, "nmol"), qty(500, "ng"))
  
  # molarity / pressure = solubility
  expect_equal(qty(10, "mM") / qty(200, "mbar"), qty(50, "mM/bar"))
  # molarity / solubility = pressure
  expect_equal(qty(10, "mM") / qty(50, "mM/bar"), qty(200, "mbar"))
  # solubility * pressure = molarity
  expect_equal(qty(50, "mM/bar") * qty (200, "mbar"), qty(10, "mM"))
  
})