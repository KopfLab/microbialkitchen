context("Conversions")

test_that("Testing that standard vector operations are working", {
  
  # subsetting
  expect_equal(qty(1:5, "g")[2:3], qty(2:3, "g"))
  expect_equal(qty(1:5, "g")[-5], qty(1:4, "g"))
  expect_equal(qty(1:5, "g")[10], qty(NA_real_, "g"))
  expect_equal(qty(1:5, "g")[-c(1:5)], qty(numeric(0), "g"))
  
  # combining
  expect_equal(cht_c_qty(qty(5, "g"), qty(c(10, 20), "mg")), qty(c(5000, 10, 20), "mg"))
  expect_equal(c(qty(5, "g"), qty(c(10, 20), "mg")), qty(c(5000, 10, 20), "mg"))
  expect_equal(c(qty(NA, "g"), qty(c(10, 20), "mg")), qty(c(NA, 10, 20), "mg"))
  expect_equal(c(qty(numeric(0), "g"), qty(c(10, 20), "mg")), qty(c(10, 20), "mg"))
})

test_that("Testing that arithemtic calculations of units are working", {
  
  # comparisons
  expect_error(cht_qty(2, "C") > cht_qty(1, "g"), "comparison is not implemented for these quantities")
  expect_true(cht_qty(100, "mg") < cht_qty(1, "g"))
  expect_true(cht_qty(100, "mg") <= cht_qty(0.1, "g"))
  expect_true(cht_qty(100, "mg") > cht_qty(10000, "µg"))
  expect_true(cht_qty(10, "mg") >= cht_qty(10000, "µg"))
  expect_true(cht_qty(10, "mg") == cht_qty(10000, "µg"))
  expect_false(cht_qty(10, "mg") != cht_qty(10000, "µg"))
  
  # addition
  expect_error(cht_qty(2, "C") + cht_qty(1, "g"), "addition is not implemented for these quantities")
  expect_error(cht_qty(2, "C") - cht_qty(1, "g"), "addition is not implemented for these quantities")
  expect_equal({q <- cht_qty(1, "mg") + cht_qty(5, "µg") - cht_qty(0.0003, "g"); q@unit}, "µg")
  expect_equal(round(q@.Data), 705) # there's a rounding problem wiht machine error otherwise
  
  # division
  expect_error(cht_qty(2, "C") / cht_qty(1, "g"), "division is not implemented for these quantities")
  expect_error(1000 / cht_qty(1, "g"), "division is not implemented for these quantities")
  expect_equal(cht_qty(1, "g") / 1000, cht_qty(1, "mg")) # auto-converts units
  expect_equal(cht_qty(1, "g") / cht_qty(1, "mg"), 1000) # should give plain number
  expect_equal(cht_qty(5, "mM") / cht_qty(1, "M"), 0.005) # should give plain number
  
  # multiplication
  expect_error(cht_qty(1, "g") * cht_qty(1, "g"), "multiplication is not implemented for these quantities")
  expect_equal(cht_qty(5, "g") * 100, cht_qty(500, "g"))
  expect_equal(100 * cht_qty(5, "g"), cht_qty(500, "g"))
  
  # amount / volume = concentration
  expect_equal(cht_qty(5, "µmol") / cht_qty(1, "ml"), cht_qty(5, "mM"))
  expect_equal(cht_qty(5, "pmol") / cht_qty(10, "nL"), cht_qty(500, "µM"))
  # amount / molarity = volume
  expect_equal(cht_qty(5, "nmol") / cht_qty(10, "mM"), cht_qty(500, "nL"))
  # volume * molarity = amount
  expect_equal(cht_qty(1, "ml") * cht_qty(10, "mM"), cht_qty(10, "µmol"))
  expect_equal(cht_qty(10, "mM") * cht_qty(1, "ml"), cht_qty(10, "µmol"))
  # chain: volume * molarity / volume = amount
  expect_equal(cht_qty(1, "ml") * cht_qty(10, "mM") / cht_qty(1, "L"), cht_qty(10, "µM"))
  expect_equal(cht_qty(1, "ml") * cht_qty(0.5, "mM") / cht_qty(100, "mM"), cht_qty(5, "µL"))
  
  # mass / MW = amount
  expect_equal(cht_qty(5, "mg") / cht_qty(100, "g/mol"), cht_qty(50, "µmol"))
  # mass / amount = MW
  expect_equal(cht_qty(2, "mg") / cht_qty(1, "µmol"), cht_qty(2000, "g/mol"))
  # amount * MW = mass
  expect_equal(cht_qty(10, "nmol") * cht_qty(50, "g/mol"), cht_qty(500, "ng"))
  
})