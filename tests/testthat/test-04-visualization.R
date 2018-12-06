context("Visualization")

test_that("test that quantity labeller works", {
  
  expect_error(make_qty_text_labeller(), "requires a quantity")
  expect_error(make_qty_text_labeller(1), "requires a quantity")
  q <- qty(c(0.1, 1, 1000), "g")
  expect_is(l <- make_qty_text_labeller(q), "labeller")
  expect_equal(l(q), c("100 mg", "1 g", "1 kg"))
  expect_is(l <- make_qty_text_labeller(q, unit = "g"), "labeller")
  expect_equal(l(q), c("0.1 g", "1 g", "1000 g"))
  
})