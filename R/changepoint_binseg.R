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
#' @param variance (char) if TRUE, then identifies the changepoints
#'   of variance of the outcome `variable` (default is considering the
#'   mean).
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
#' changepoint_binseg(variable, time, change_points)
#' # Identify the 3 change points of the variance of variable using
#' # BinSeg algorithm
#' changepoint_binseg(variable, time, change_points, variance = TRUE)
changepoint_binseg <- function(variable, time, change_points,
                                    variance=TRUE) {

  stopifnot(
    `change_point must be a single number` = length(change_points) == 1,
    `variable and time must have same length` =
      length(variable) == length(time)
  )


  cpt_mean <- changepoint::cpt.mean(
    variable,
    method = "BinSeg",
    Q = change_points
  )

  cpt_var <- changepoint::cpt.var(
    variable,
    method = "BinSeg",
    Q = change_points
  )

  if(variance){
    sort(unlist(time[changepoint::cpts(cpt_var)]))
  }
  if(!variance){
    sort(unlist(time[changepoint::cpts(cpt_mean)]))
  }

}