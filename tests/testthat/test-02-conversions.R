context("Conversions")

test_that("Testing that arithemtic calculations of units are working", {
  
  # comparisons
  expect_error(qty(2, "C") > qty(1, "g"), "comparison is not implemented for these quantities")
  expect_true(qty(100, "mg") < qty(1, "g"))
  expect_true(qty(100, "mg") <= qty(0.1, "g"))
  expect_true(qty(100, "mg") > qty(10000, "µg"))
  expect_true(qty(10, "mg") >= qty(10000, "µg"))
  
  # addition
  expect_error(qty(2, "C") + qty(1, "g"), "addition is not implemented for these quantities")
  expect_error(qty(2, "C") - qty(1, "g"), "addition is not implemented for these quantities")
  expect_equal({q <- qty(1, "mg") + qty(5, "µg") - qty(0.0003, "g"); q@unit}, "µg")
  expect_equal(round(q@.Data), 705) # there's a rounding problem wiht machine error otherwise
  
  # division
  expect_error(qty(2, "C") / qty(1, "g"), "division is not implemented for these quantities")
  expect_equal(qty(1, "g") / qty(1, "mg"), 1000) # should give plain number
  expect_equal(qty(5, "mM") / qty(1, "M"), 0.005) # should give plain number
  
  # amount / volume = concentration (all permutations)
  expect_equal(qty(5, "µmol") / qty(1, "ml"), qty(5, "mM"))
  expect_equal(qty(5, "pmol") / qty(10, "nL"), qty(500, "µM"))
  expect_equal(qty(5, "nmol") / qty(10, "mM"), qty(500, "nL"))
  expect_equal(qty(1, "ml") * qty(10, "mM"), qty(10, "µmol"))
  expect_equal(qty(10, "mM") * qty(1, "ml"), qty(10, "µmol"))
  expect_equal(qty(1, "ml") * qty(10, "mM") / qty(1, "L"), qty(10, "µM"))
  expect_equal(qty(1, "ml") * qty(0.5, "mM") / qty(100, "mM"), qty(5, "µL"))
  
  # mass / MW = amount (all permutations)
  expect_equal(qty(5, "mg") / qty(100, "g/mol"), qty(50, "µmol"))
  expect_equal(qty(2, "mg") / qty(1, "µmol"), qty(2000, "g/mol"))
  expect_equal(qty(10, "nmol") * qty(50, "g/mol"), qty(500, "ng"))
  
  # combining equations
  
})