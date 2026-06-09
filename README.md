
<!-- README.md is generated from README.Rmd. Please edit that file -->

# semID

<!-- badges: start -->

[![R-CMD-check](https://github.com/zacharyvig/semID/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zacharyvig/semID/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

semID allows the user to input a Structural Equation Model (SEM) in
`lavaan` (Roseel, 2012) syntax and check it against a number of
identification rules from the literature. Rules are specified as being
necessary and/or sufficient and specific reasons are given when a rule
is broken. Caution is given to the user in using the output of the
package as the sole determinant of model identification – instead, it
should be used as a “quick check” for any outstanding issues with the
model.

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
#> semID 0.3.0 is still in the development phase

# Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
id(HS.model, include.msgs = TRUE, call = "cfa", 
   meanstructure = FALSE)
#>                        Pass Necessary Sufficient Message 
#> Three Indicator Rule    Yes        No        Yes         
#> Two Indicator Rule      Yes        No        Yes         
#> Fully Recursive Rule      -         -          -       1 
#> Null B_YY Rule            -         -          -       1 
#> Recur/Corr Err Rule       -         -          -       1 
#> Exogenous X Rule          -         -          -       2 
#> Latent Scaling Rule     Yes       Yes         No 
#> N_theta Rule            Yes       Yes         No 
#> 2+ Emitted Paths Rule   Yes       Yes         No 
#> ---
#> Messages
#> 1 - [Info] This rule only applies when there are no
#>     latent variables in the model
#> 2 - [Info] This rule only applies when causal
#>     indicators are in the model
```
