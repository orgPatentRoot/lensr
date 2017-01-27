#tests for lens iterate
context("lens_iterate function")

test_that("single url works", {
  lita <- lens_iterate(one_url, lens_parse)
  expect_is(lita, "list")
})

test_that("three urls works", {
  litb <- lens_iterate(three_urls, lens_parse)
  expect_is(litb, "list")
})