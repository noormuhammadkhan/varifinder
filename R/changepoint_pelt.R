#' Changepoint identification with PELT algorithm
#'
#' Identify the changepoints, where the mean or variance of the
#' response variable changes  using the Pruned Exact Linear Time
#'  `(PELT)` algorithm.
#'
#'
#' @param variable (num) The outcome variable.
#' @param time (num) The time point(s) in which the outcome
#' `variable` is measured.
#' @param stat (char) if variance, then identifies the changepoints
#' of variance of the outcome `variable` (default is considering the
#' mean).
#' @return (num) vector of length `change_points` reporting the
#'   timepoints when the changes happens.
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#'
#' # Identify the change points of the mean of variable using PELT
#' # algorithm
#'  changepoint_pelt(variable, time)
#' # Identify the change points of the variance of variable using
#' # PELT algorithm
#'  changepoint_pelt(variable, time, stat = "variance")
changepoint_pelt <- function(variable, time,
                             stat = c("mean", "variance")) {

  stopifnot(
    `variable and time must have same length` =
      length(variable) == length(time)
  )


  stat <- match.arg(stat)

  switch(
    stat,
    "mean" = changepoint_pelt_mean(
      variable = variable,
      time = time),
    "variance" = changepoint_pelt_var(
      variable = variable,
      time = time)
  )

}

