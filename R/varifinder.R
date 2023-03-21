#' Variability Ratio Index (VARI)
#'
#' Calculate the Variability Ratio Index (VARI) index.
#'
#' @param variable (num) The outcome variable in long format.
#' @param phase (num) vector of clinical phases of the total time
#'  in long format.
#' @param time (num) vector of time points, when the outcome is measured
#'  in long format.
#'
#' @return (num) data frame containing rows for the the unique phases.
#' Times points, change points and VARI based on mean and variance for
#' each phase.
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

varifinder <- function(phase, variable, time){

  j = 0
  j_rows = list()
  Time = NULL
  CPT_M <- NULL
  CPT_V <- NULL

  for (i in 1:length(unique(phase))) {


    j_rows  <- seq(j + 1, j + as.matrix(summary(phase))[i])
    j <- j + as.matrix(summary(phase))[i]

    Time [i] <- (length(j_rows))


    CPT_M [i] <- (length(changepoint_pelt_mean(variable[j_rows], time[j_rows])))


    CPT_V [i] <- (length(changepoint_pelt_var(variable[j_rows], time[j_rows])))


    Phase <- seq_along(unique(phase))

  }

  VARI_M <- CPT_M / Time
  VARI_V <- CPT_V / Time

  return(data.frame(Phase, Time, CPT_M, VARI_M, CPT_V, VARI_V))
}


