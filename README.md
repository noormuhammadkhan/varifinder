
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varifinder

<!-- badges: start -->
<!-- badges: end -->

The goal of varifinder is to calculate VARI to compare the change points
in retrospective data.

## Installation

You can install the development version of varifinder from
[GitHub](https://github.com/) with: (noormuhammadkhan)

``` r
# install.packages("devtools")
devtools::install_github("noormuhammadkhan/varifinder")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(varifinder)
 # Prepare a dummy data set
 variable <- rnorm(100, 50, 5)
 time <- c(seq(1,30), seq(1, 40), seq(1,20), seq(1,10))
 phase <- rep(factor(c(1, 2, 3, 4)), c(30, 40, 20, 10))
 
 # Identify the VARI
 varifinder(variable, phase, time)
```
