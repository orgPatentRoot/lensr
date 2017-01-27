context("lens_count function")
#note to change to data.frame or tibble rather than a list. At present only passes on list and should be tibble.
# test_that("count families TRUE works", {
#   f <- lens_count("drones", families = TRUE, timer= 5)
#   expect_type(f, "list")
#   expect_length(f, 2)
#   #expect_identical(f$families, f$search) #expect that families count is less than publication count
# }
#   )

test_that("count families FALSE single term works", {
  t <- lens_count("drones", families = FALSE, timer = 5)
  expect_type(t, "list")
  expect_length(t, 3)
}
)

test_that("no boolean vector of terms work", {
  t <- lens_count(synbio, timer = 5)
  expect_type(t, "list")
  expect_length(t, 3)
  expect_equal(nrow(t) , expected = 7)
}
)

test_that("single term with zero results works where families set to TRUE", {
  t <- lens_count("lake baringo", families = TRUE, timer = 5)
  expect_type(t, "list")
  expect_length(t, 3)
  expect_equal(nrow(t) , expected = 1)
}
)

test_that("single term with results and families where families set to TRUE works", {
  t <- lens_count("synthetic biology", families = TRUE, timer = 5)
  expect_type(t, "list")
  expect_length(t, 3)
  expect_equal(nrow(t) , expected = 1)
}
)

test_that("zero results works on vector of terms", {
  t <- lens_count(c("lake naivasha", "lake baringo"), timer = 5)
  expect_type(t, "list")
  expect_length(t, 3)
  expect_equal(nrow(t) , expected = 2)
}
)

test_that("vector of terms with boolean OR works", {
  v <- lens_count(c("drone", "drones"), boolean = "OR", timer = 5)
  expect_type(v, "list")
  expect_length(v, 3)
  expect_equal(nrow(v), expected = 1)
})

test_that("vector of terms with boolean AND works", {
  v <- lens_count(c("drone", "drones"), boolean = "AND", timer = 5)
  expect_type(v, "list")
  expect_length(v, 3)
  expect_equal(nrow(v), expected = 1)
})

test_that("families detect works where detect fails and transfer from publications", {
  v <- lens_count("donald duck", jurisdiction = "EP", families = TRUE)
  expect_type(v, "list")
  expect_length(v, 3)
})