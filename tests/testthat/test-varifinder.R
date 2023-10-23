test_that("varifinder works on an example", {
  # Prepare a dummy data set
  variable <- rnorm(100, 50, 5)
  time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
  phase <- rep(factor(c(1, 2, 3, 4)), c(30, 40, 20, 10))

  # Identify the VARI
  expect_data_frame(
    varifinder(variable, phase, time)
  )
})
