# Environment to store rules that apply to all models
.any_rules <- new.env(parent = emptyenv())

#' N_theta rule (compares parameters to observed statistics)
#' @name ntheta_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @importFrom lavaan lav_partable_npar lav_partable_ndat
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
#' @author Zach Vig
.any_rules$ntheta_rule <- function(partable) {
  # number of parameters
  npar <- lav_partable_npar(partable)
  # number of means, variances, and covariances
  ndat <- lav_partable_ndat(partable)
  # build output
  rule <- "N_theta Rule"
  pass <- isTRUE(npar <= ndat)
  warn <- ifelse(
    pass, NA,
    sprintf("The number of free parameters (%s) exceeds the number of means/variances/covariances (%s)", npar, ndat)
  )
  out <- list(
    rule = rule,
    pass = pass,
    warn = warn,
    cond = "N"
  )
  return(out)
}
