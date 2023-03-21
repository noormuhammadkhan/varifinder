#' Changepoint identification with Binary Segmentation algorithm
#'
#' Identify the changepoints, where the mean or variance of the
#' response variable changes  using Binary Segmentation algorithm.
#'
#'
#' @param variable (num) The outcome variable.
#' @param time (num) The time point(s) in which the outcome `variable`
#'   is measured.
#' @param change_points (num) Single number of changepoint that
#'   are expecting
#' @param stat (char) if variance, then identifies the changepoints
#'   of variance of the outcome `variable` (default is considering the
#'   mean).
#'
#'
#' @return (num) vector of length `change_points` reporting the
#'   timepoints when the changes happens.
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#' change_points <- 3
#'
#' # Identify the 3 change points of the mean of variable using BinSeg
#' # algorithm
#'  changepoint_binseg(variable, time, change_points)
#' # Identify the 3 change points of the variance of variable using
#' # BinSeg algorithm
#'  changepoint_binseg(variable, time, change_points, stat = "variance")
changepoint_binseg <- function(variable, time, change_points,
                               stat = c("mean", "variance")) {

  stopifnot(
    `change_point must be a single number` = length(change_points) == 1,
    `variable and time must have same length` =
      length(variable) == length(time)
  )


  stat <- match.arg(stat)

  switch(
    stat,
    "mean" = changepoint_binseg_mean(
      variable = variable,
      time = time,
      change_points = change_points),
    "variance" = changepoint_binseg_mean(
      variable = variable,
      time = time,
      change_points = change_points)
  )

 }

