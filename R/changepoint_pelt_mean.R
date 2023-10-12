#' Change point identification with PELT algorithm
#'
#' Identify the change points, where the mean of the response variable
#'  changes  using the Pruned Exact Linear Time `(PELT)`  algorithm.
#'
#'
#' @param variable (num) The outcome variable.
#' @param time (num) The time point(s) in which the outcome `variable`
#'   is measured.
#'
#' @return (num) vector of length `change_points` reporting the
#'   time points when the changes happen.
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#'
#' # Identify the change points of the mean of variable using pelt
#' # algorithm
#' changepoint_pelt_mean(variable, time)
changepoint_pelt_mean <- function(variable, time) {

  stopifnot(
    `variable and time must have same length` =
      length(variable) == length(time)
  )


  cpt_mean <- changepoint::cpt.mean(
    variable, 
    penalty = "AIC",
    method = "PELT"
    )

  sort(changepoint::cpts(cpt_mean))

}
