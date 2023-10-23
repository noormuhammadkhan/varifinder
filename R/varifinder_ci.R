#' Confidence Interval of the Variability Ratio Index.
#'
#' Identify the 95% exact confidence interval for the Variability Ratio
#' Index (VARI).
#'
#'
#' @param time (num) The time point(s) in which the outcome variable
#'   is measured.
#'
#' @param cpt (num) The number of change points detected within each
#' phase.
#'
#' @param vari (num) Variability Ratio Index.
#'
#' @return (num) data frame containing rows for the unique phases.
#' The column `Times` presents the total number of time points in
#' within  that phase. `VARI` presents the Variability
#' Ratio Index for each phase. The `Lower_limit` and the `Upper_limit`
#' presents the 95% exact confidence interval for the VARI.
#'
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#' phase <- rep(factor(c(1, 2, 3, 4)), c(30, 40, 20, 10))
#'
#'
#' # Identify the 95% exact confidence interval for VARI when it is
#' # calculated using mean function
#' vari <- varifinder(variable, phase, time)
#' varifinder_ci(vari$Time, vari$CPT_M, vari$VARI_M)
#'
#' # Identify the 95% exact confidence interval for VARI when it is
#' # calculated using variance function
#' varifinder_ci(vari$Time, vari$CPT_V, vari$VARI_V)


varifinder_ci <- function(time, cpt, vari) {
  exact_lower <- NULL
  exact_upper <- NULL

  for (i in seq_along(cpt)) {
    # Generate the confidence interval
    exact_lower[i] <- binom::binom.confint(
      cpt[i], time[i],
      conf.level = 0.95,
      method = "exact"
    )$lower
    exact_upper[i] <- binom::binom.confint(
      cpt[i], time[i],
      conf.level = 0.95,
      method = "exact"
    )$upper
  }

  data.frame(
    time = time,
    vari = vari,
    lower_limit = exact_lower,
    upper_limit = exact_upper
  )
}
