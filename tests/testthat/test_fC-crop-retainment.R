test_that("fC_crop_retainment works for length 1 inputs", {
  res <- fC_crop_retainment(TRUE)
  expect_length(res, 12)
})

test_that("fC_crop_retainment works for length 1 inputs", {
  res <- fC_crop_retainment(rep(c(TRUE, FALSE), each = 6))
  expect_equal(
    res,
    rep(c(0.6, 1.0), each = 6),
    tolerance = 2 * testthat_tolerance()
  )
})
