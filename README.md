
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
devtools::install_github("zacharyvig/semID")
#> Downloading GitHub repo zacharyvig/semID@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\zachv\AppData\Local\Temp\Rtmp46lquv\remotes2f502efa4e9\zacharyvig-semID-d2bf617/DESCRIPTION' ...  ✔  checking for file 'C:\Users\zachv\AppData\Local\Temp\Rtmp46lquv\remotes2f502efa4e9\zacharyvig-semID-d2bf617/DESCRIPTION'
#>       ─  preparing 'semID':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>       ─  checking for empty or unneeded directories
#>      Omitted 'LazyData' from DESCRIPTION
#>       ─  building 'semID_0.2.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/zachv/AppData/Local/Temp/RtmpagTz0L/temp_libpath228083f1280'
#> (as 'lib' is unspecified)
```

## Example

``` r
library(semID)

#' # Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
id(HS.model, warn = TRUE, call = "cfa", meanstructure = FALSE)
#>                        Pass Necessary Sufficient Warning 
#> 
#> N_theta Rule            Yes       Yes         No 
#> 2+ Emitted Paths Rule   Yes       Yes         No 
#> Exogenous X Rule         NA         -          -       1 
#> Three Indicator Rule    Yes        No        Yes 
#> Two Indicator Rule      Yes        No        Yes 
#> 
#> Warnings:
#> 1 - This rule only applies when causal indicators are
#>     present
```
