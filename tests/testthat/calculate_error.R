test_that("Default difference calculation", {
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2)),
    c(1,2,1,1,1,3)
  )
})

test_that("Absolute difference calculation", {
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "Absolute"),
    c(1,2,1,1,1,3)
  )
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "absolute"),
    c(1,2,1,1,1,3)
  )
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "AbSoLuTe"),
    c(1,2,1,1,1,3)
  )
})

test_that("Logarithmic difference calculation", {
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "Logarithmic"),
    c(0.69314718, 1.09861229, 0.69314718, 0.69314718, 0.69314718, 1.38629436)
  )
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "logarithmic"),
    c(0.69314718, 1.09861229, 0.69314718, 0.69314718, 0.69314718, 1.38629436)
  )
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "lOgARITHmic"),
    c(0.69314718, 1.09861229, 0.69314718, 0.69314718, 0.69314718, 1.38629436)
  )
})

test_that("Residual difference calculation", {
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "Residual"),
    c(1,2,1,1,-1,3)
  )
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "residual"),
    c(1,2,1,1,-1,3)
  )
  expect_equal(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "ResiDual"),
    c(1,2,1,1,-1,3)
  )
})

test_that("Type exception", {
  expect_error(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "Squared"),
    "Invalid error type. Type must be Absolute, Residual or Logarithmic."
  )
  expect_error(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = "Log"),
    "Invalid error type. Type must be Absolute, Residual or Logarithmic."
  )
  expect_error(
    calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type = ""),
    "Invalid error type. Type must be Absolute, Residual or Logarithmic."
  )
})
