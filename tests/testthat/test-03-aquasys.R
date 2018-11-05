context("Aquasys")

test_that("Testing parameter operations", {
  
  # setting up parameter system
  expect_error(parameter(nonexistent = 5), "not a valid parameter")  
  expect_error(parameter(solution.volume = cht_temperature(1, "C")), "must be a Volume")
  expect_is(ps <- parameter(
    solution.volume = cht_qty(1, "L"),
    headspace.volume = cht_qty(10, "mL"),
    headspace.pressure = cht_qty(5, "psi")
  ), "aquasys")
  expect_is(ps, "parameters")
  expect_equal(ps$parameters$headspace.volume, cht_qty(10, "mL"))
  expect_error(add_parameter(ps, "test"), "can only work with aquasys objects")
  expect_error(ps + 5, "Don't know how to add")
  expect_equal( {ps2 <- ps + parameter( solution.volume = cht_qty(1, "µL")); ps2$parameters$solution.volume}, cht_qty(1, "µL"))
  
})

test_that("Testing assembly of an aquatic system", {
  
})

