#' Changepoint identification with PELT algorithm
#'
#' Identify the changepoints, where the variance of the response
#' variable changes  using the Pruned Exact Linear Time `(PELT)`
#' algorithm.
#'
#' @param variable (num) The outcome variable.
#' @param time (num) The time point(s) in which the outcome `variable`
#'   is measured.
#'
#' @return (num) vector of length `change_points` reporting the
#'   timepoints when the changes happens.
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#'
#' # Identify the change points of the variance of variable using pelt
#' # algorithm
#' changepoint_pelt_var(variable, time)
changepoint_pelt_var <- function(variable, time) {

  stopifnot(
    `variable and time must have same length` =
      length(variable) == length(time)
  )


  cpt_var <- changepoint::cpt.var(
    variable,
    method = "PELT"
  )

  sort(unlist(time[changepoint::cpts(cpt_var)]))

}
