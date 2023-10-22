#' Variability Ratio Index (VARI)
#'
#' Calculate the Variability Ratio Index (VARI) to compare the number of
#' detected change points between different clinical phases in
#' retrospective data.
#'
#' @param variable (num) The outcome variable in long format.
#' @param phase (character) list of clinical phases of the total time
#'  in long format.
#' @param time (num) vector of time points, when the outcome is measured
#'  in long format.
#'
#' @return (num) data frame containing rows for the unique phases.
#' The column `times` presents the total number of time points within
#' that phase. `cpt_m` and `cpt_v` present the total number of
#' detected change points in each phase based on mean and variance
#' function of the variable respectively. Finally, the column `vari_m`
#' and `vari_v` present the variability ratio index for each phase.
#' @export
#'
#' @examples
#' # Prepare a dummy data set
#' variable <- rnorm(100, 50, 5)
#' time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
#' phase <- rep(factor(c(1, 2, 3, 4)), c(30, 40, 20, 10))
#'
#' # Identify the VARI
#'  varifinder(variable, phase, time)

varifinder <- function(variable, phase, time) {

  j <- 0
  j_rows <- list()
  time <- NULL
  cpt_m <- NULL
  cpt_v <- NULL

  for (i in seq_along(unique(phase))) {


    j_rows  <- seq(j + 1, j + as.matrix(summary(phase))[i])
    j <- j + as.matrix(summary(phase))[i]

    time[i] <- (length(j_rows))


    cpt_m[i] <- (length(changepoint_pelt_mean(variable[j_rows], time[j_rows])))


    cpt_v[i] <- (length(changepoint_pelt_var(variable[j_rows], time[j_rows])))


    phase <- seq_along(unique(phase))

  }

  vari_m <- cpt_m / time
  vari_v <- cpt_v / time

  return(data.frame(phase, time, cpt_m, vari_m, cpt_v, vari_v))
}
