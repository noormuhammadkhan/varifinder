test_that("Error if chengepoints is not a single number", {
  # setup
  var <- sample(8)
  time <- sample(6)
  change_points <- c(1, 2)

  # test
  expect_error(
    changepoint_binseg_mean(var, time, change_points),
    "must be a single number"
  )

})

test_that("variable and time have same length", {
  # setup
  var <- sample(8)
  time <- sample(6)
  change_points <- 2

  # test
  expect_error(changepoint_binseg_mean(var, time, change_points))

})
