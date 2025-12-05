
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semID

<!-- badges: start -->

[![R-CMD-check](https://github.com/zacharyvig/semID/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zacharyvig/semID/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

semID allows the user to input a Structural Equation Model (SEM) in
`lavaan` syntax and check it against a number of identification rules
from the literature. Rules are specified as being necessary and/or
sufficient and specific reasons are given when a rule is broken. Caution
is given to the user in using the output of the package as the sole
determinant of model identification – instead, it should be used as a
“quick check” for any outstanding issues with the model.

## Installation

You can install the development version of semID from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("zacharyvig/semID")
```

## Example

``` r
library(semID)
#> semID 0.2.1.9003 is still in the development phase

# Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
id(HS.model, warn = TRUE, call = "cfa", meanstructure = FALSE)
#>                        Pass Necessary Sufficient Warning 
#> N_theta Rule            Yes       Yes         No 
#> Latent Scaling Rule     Yes       Yes         No 
#> 2+ Emitted Paths Rule   Yes       Yes         No 
#> Exogenous X Rule          -         -          -       1 
#> Three Indicator Rule    Yes        No        Yes 
#> Two Indicator Rule      Yes        No        Yes 
#> Null B_YY Rule            -         -          -       2 
#> Fully Recursive Rule      -         -          -       2 
#> Recur/Corr Err Rule       -         -          -       2 
#> ---
#> Warnings
#> 1 - This rule only applies when causal indicators are
#>     in the model
#> 2 - This rule only applies when there are no latent
#>     variables in the model
```
