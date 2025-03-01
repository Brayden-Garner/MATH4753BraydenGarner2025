test_that("myncurve function works", {
  l <- myncurve(mu=10, sigma=5, a=6)
  expect_equal(l[[1]], 10)
  expect_equal(l[[2]], 5)
  expect_equal(l[[3]], 0.21185540)
})
