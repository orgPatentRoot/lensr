#tests for lens_urls
context("lens_urls function")

test_that("results argument works correctly", {
  lua <- lens_urls("synthetic biology", families = TRUE, results = 20)
  expect_identical(lua, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&f=true&st=false&n=50")
})

test_that("NULL in results returns 50", {
  lub <- lens_urls("synthetic biology", type = "tac", families = TRUE)
  expect_identical(lub, "https://www.lens.org/lens/search?q=%28title%3A%28%22synthetic+biology%22%29+%7C%7C+abstract%3A%28%22synthetic+biology%22%29+%7C%7C+claims%3A%28%22synthetic+biology%22%29%29&f=true&st=false&n=50")
})

test_that("pub_date_start and end work",{
  luc <- lens_urls("synthetic biology", publn_date_start = 19900101, publn_date_end = 20150101, families = TRUE)
  expect_identical(luc, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&dates=%2Bpub_date%3A19900101-20150101&f=true&st=false&n=50")
})

test_that("filing_date_start and end work",{
  lud <- lens_urls("synthetic biology", filing_date_start = 19900101, filing_date_end = 20150101, families = TRUE)
  expect_identical(lud, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&dates=%2Bfiling_date%3A19900101-20150101&f=true&st=false&n=50")
})

# test_that("short pub year to date conversion works", {
#   lue <- lens_urls("synthetic biology", pub_date_start = 1990, pub_date_end = 2015)
#   expect_identical(lue, "https://www.lens.org/lens/search?q=%22synthetic+biology%22&dates=%2Bpub_date%3A19900101-20151231&n=50&f=true")
#
# })

# boolean and type tests go here

test_that("boolean OR works with upto 50 results", {
  lua <- lens_urls(query = c("drones", "drone"), boolean = "OR")
  expect_identical(lua, "https://www.lens.org/lens/search?q=%22drones%22+%7C%7C+%22drone%22&st=false&n=50")
})

test_that("boolean OR works with upto 150 results", {
  lua <- lens_urls(query = c("drones", "drone"), boolean = "OR", results = 150)
  lub <- c("https://www.lens.org/lens/search?p=0&q=%22drones%22+%7C%7C+%22drone%22&st=false&n=50", "https://www.lens.org/lens/search?p=1&q=%22drones%22+%7C%7C+%22drone%22&st=false&n=50", "https://www.lens.org/lens/search?p=2&q=%22drones%22+%7C%7C+%22drone%22&st=false&n=50")
  expect_identical(lua, lub)
})


test_that("boolean AND works with upto 50 results", {
  lua <- lens_urls(query = c("drones", "drone"), boolean = "AND")
  expect_identical(lua, "https://www.lens.org/lens/search?q=%22drones%22+%26%26+%22drone%22&st=false&n=50")
})

# results filter and pager tests
test_that("null results works", {
  lua <- lens_urls(query = "drones")
  expect_identical(lua, "https://www.lens.org/lens/search?q=%22drones%22&st=false&n=50")
})

test_that("specifying 50 results works", {
  lub <- lens_urls(query = "drones", results = 50)
  expect_identical(lub, "https://www.lens.org/lens/search?q=%22drones%22&st=false&n=50")
})

test_that("specifying less than 50 results works", {
  luc <- lens_urls(query = "drones", results = 20)
  expect_identical(luc, "https://www.lens.org/lens/search?q=%22drones%22&st=false&n=50")
})

test_that("specifying over 50 results works", {
  lud <- lens_urls(query = "drones", results = 150)
  ludl <- c("https://www.lens.org/lens/search?p=0&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=1&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=2&q=%22drones%22&st=false&n=50")
  expect_identical(lud, ludl)
})

test_that("specifying upto 500 results works", {
  lud <- lens_urls(query = "drones", results = 500)
  ludv <- c("https://www.lens.org/lens/search?p=0&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=1&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=2&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=3&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=4&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=5&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=6&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=7&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=8&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=9&q=%22drones%22&st=false&n=50")
  expect_identical(lud, ludv)
})

test_that("odd numbers are rounded in url generation", {
  lue <- lens_urls(query = "drones", results = 175)
  luel <- c("https://www.lens.org/lens/search?p=0&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=1&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=2&q=%22drones%22&st=false&n=50", "https://www.lens.org/lens/search?p=3&q=%22drones%22&st=false&n=50")
  expect_identical(lue, luel)
})

# test_that("specifying over 500 results creates message", {
# lue <- lens_urls(query = "drones", results = 700)
# #lum <- c("More than 500 families, only 500 can be returned. Try using date ranges in lens_count to break the data into chunks of 500 or less")
# #   expect_message(lue, lum, ignore.case = TRUE)
# # })

#Jurisdictions tests

test_that("main jurisdiction group works", {
  lue <- lens_urls(query = "drones", jurisdiction = "main")
  luej <- c("https://www.lens.org/lens/search?q=%22drones%22&jo=true&j=EP&j=JP&j=US&j=WO&st=false&n=50")
  expect_identical(lue, luej)
})

test_that("main jurisdiction group works with family", {
  lue <- lens_urls(query = "drones", jurisdiction = "US", families = TRUE)
  luej <- c("https://www.lens.org/lens/search?q=%22drones%22&jo=true&j=US&f=true&st=false&n=50")
  expect_identical(lue, luej)
})

test_that("ops jurisdiction group works with family", {
  lue <- lens_urls(query = "drones", jurisdiction = "ops", families = TRUE)
  luej <- c("https://www.lens.org/lens/search?q=%22drones%22&jo=true&j=AT&j=CA&j=CH&j=EP&j=GB&j=WO&f=true&st=false&n=50")
  expect_identical(lue, luej)
})
