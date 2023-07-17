#' Change point identification with Binary Segmentation algorithm
#'
#' Identify the change points, where the mean of the response variable
#' changes  using Binary Segmentation algorithm.
#'
#'
#' @param variable (num) The outcome variable.
#' @param time (num) The time point(s) in which the outcome `variable`
#'   is measured.
#' @param change_points (num) Single number of change point that
#'   are expected
#'
#' @return (num) vector of length `change_points` reporting the
#'   time points when the changes happen.
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
#' changepoint_binseg_mean(variable, time, change_points)
changepoint_binseg_mean <- function(variable, time, change_points) {

  stopifnot(
    `change_point must be a single number` = length(change_points) == 1,
    `variable and time must have same length` =
      length(variable) == length(time)
  )


  cpt_mean <- changepoint::cpt.mean(
    variable,
    penalty = "BIC",
    method = "BinSeg",
    Q = change_points
  )

  sort(unlist(time[changepoint::cpts(cpt_mean)]))

}
