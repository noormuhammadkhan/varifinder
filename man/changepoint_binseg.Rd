% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changepoint_binseg.R
\name{changepoint_binseg}
\alias{changepoint_binseg}
\title{Change point identification with Binary Segmentation algorithm.}
\usage{
changepoint_binseg(variable, time, change_points, stat = c("mean", "variance"))
}
\arguments{
\item{variable}{(num) The outcome variable.}

\item{time}{(num) The time point(s) in which the outcome \code{variable}
is measured.}

\item{change_points}{(num) Single number of change point that
are expecting}

\item{stat}{(char) if variance, then identifies the time points, when
the variance of the outcome \code{variable} changes.
(default is considering the mean).}
}
\value{
(num) vector of length \code{change_points} reporting the
time points when the changes happen.
}
\description{
Identify the time points, when the mean or variance of the
response variable changes using Binary Segmentation algorithm.
}
\examples{
# Prepare a dummy data set
variable <- rnorm(100, 50, 5)
time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
change_points <- 3

# Identify the 3 change points of the mean of variable using BinSeg
# algorithm
 changepoint_binseg(variable, time, change_points)
# Identify the 3 change points of the variance of variable using
# BinSeg algorithm
 changepoint_binseg(variable, time, change_points, stat = "variance")
}
