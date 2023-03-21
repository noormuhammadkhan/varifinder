test_that("variable and time have same length", {
  # setup
  var <- sample(8)
  time <- sample(6)

  # test
  expect_error(changepoint_pelt_var(var, time))

})
