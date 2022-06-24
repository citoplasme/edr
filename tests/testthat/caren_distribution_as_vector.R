test_that("Basic behavior", {
  expect_equal(
    caren_distribution_as_vector("error={1/2,2.9753689/1,3/4}"),
    c(1,1,2.9753689,3,3,3,3)
  )
})
