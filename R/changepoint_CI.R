#' Confidence Interval of mean difference at the Change point.
#'
#' Calculate the 95% confidence interval for the differences at the
#' time points, when the mean value of the variable differ.
#'
#' @param variable (num) The outcome variable.
#' @param changepoints (num) vector of length `change_points` reporting
#'  the timepoints when the changes happens.
#' @return (num) data frame containing the column `Change_Points` that
#' presents the time points when the mean of the variable changes.
#' The column `Mean_difference` presents the amount of mean differs at
#' the change point with 95% confidence interval in the column
#' `Lower_Bound` and `Upper_Bound`.
#'
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#' change_points <- 3
#'
#' # Identify the change points of the mean of variable using BinSeg
#' # algorithm
#'  cpt_binseg <- changepoint_binseg(variable, time, change_points)
#'  changepoint_ci(variable, cpt_binseg)
#' # Identify the change points of the mean of variable using
#' # PELT algorithm
#'  cpt_pelt <- changepoint_pelt(variable, time)
#'  changepoint_ci(variable, cpt_pelt)
changepoint_ci <- function(variable, changepoints){

    mean_difference <- NULL
  for (i in 1:length(changepoints)) {
    mean_difference[i] <- mean(stats::na.omit(variable[changepoints[i] +
                                                  1:length(variable)]))
    - mean(stats::na.omit(variable[1:(changepoints[i])]))
  }



  n <- length(variable)
  t_value <- stats::qt(0.975, n - changepoints - 1)

  standard_error <- NULL
  for (i in 1:length(changepoints)) {
    standard_error[i] <- sqrt(stats::var(stats::na.omit(variable[(changepoints[i] + 1):length(variable)])) / (n - changepoints[i]) + stats::var(stats::na.omit(variable[1:(changepoints[i])])) / changepoints[i])
  }




  CI_lower <- mean_difference - t_value * standard_error
  CI_upper <- mean_difference + t_value * standard_error




  data.frame(
    Change_Points = changepoints,
    Mean_Difference = mean_difference,
    Lower_Bound = CI_lower,
    Upper_Bound = CI_upper
    )

}

