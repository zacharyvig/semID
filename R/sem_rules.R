# Environment to store general SEM rules
.sem_rules <- list()

#' N_theta rule (compares parameters to observed statistics)
#' @name ntheta_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @importFrom lavaan lav_partable_npar lav_partable_ndat
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
#' @author Zach Vig
.sem_rules$ntheta_rule <- function(partable) {
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

#' 2+ Emitted Paths rule
#' @name two_emitted_paths_rule
#'
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Bollen & Davis (2009). Two Rules of Identification for Structural Equation Models.
#' @keywords internal
#' @author Zach Vig
.sem_rules$two_emitted_paths_rule <- function(partable) {
  rule <- "2+ Emitted Paths Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # tally variables assuming one block
  lv <- vnames$lv[[1]]
  if (length(lv) == 0) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "The model contains no latent variables"
    )
    return(out)
  }
  # build output
  free_var <- sapply(lv, function(var) {
    c1 <- isTRUE(
      with(partable, free[lhs == var & rhs == var & op == "~~"]) > 0
    )
    return(c1)
  })
  free_var.nox <- sapply(lv, function(var) {
    nox <- c(
      with(partable, rhs[lhs == var & op == "=~"]),
      with(partable, lhs[rhs == var & op == "~"])
    )
    c2 <- sapply(nox, function(var.nox) {
      isTRUE(
        with(
          partable,
          free[lhs == var.nox & rhs == var.nox & op == "~~"]) > 0
      )
    }, simplify = TRUE)
    return(all(c2))
  })
  two_path.lv <- sapply(lv, function(var) {
    c3 <- sum(
      with(
        partable,
        lhs == var & op == "=~" | rhs == var & op == "~"
      )
    )
    return(c3 >= 2)
  })
  if (all(!free_var | !free_var.nox)) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "There are no applicable variables (free variance & free downstream disturbance variances) to which to apply the rule"
    )
    return(out)
  } else {
    pass <- isTRUE(all(lv[free_var & free_var.nox] == lv[two_path.lv]))
    cond <- "N"
  }
  # warnings
  warn <- c()
  if (!pass) {
    warn <- c(
      warn,
      paste(
        "Some variables do not have two emitted paths:",
        lv[free_var & free_var.nox & !two_path.lv]
      )
    )
  }
  if (any(!free_var)) {
    warn <- c(
      warn,
      paste(
        "(Not a warning) This rule ignores latent variables without free variance:",
        lv[!free_var]
      )
    )
  }
  if (any(!free_var.nox)) {
    warn <- c(
      warn,
      paste(
        "(Not a warning) This rule ignores latent variables with downstream variables that do not have free disturbance variances:",
        lv[!free_var.nox]
      )
    )
  }
  if (length(warn) == 0) warn <- NA
  out <- list(
    rule = rule,
    pass = pass,
    cond = cond,
    warn = warn
  )
  return(out)
}

#' Exogenous X rule/MIMIC rules
#' @name exogenous_x_rule
#'
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Bollen & Davis (2009). Two Rules of Identification for Structural Equation Models.
#' @keywords internal
#' @author Zach Vig
.sem_rules$exogenous_x_rule <- function(partable) {
  rule <- "Exogenous X Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # tally variables assuming one block
  lv <- vnames$lv[[1]]
  ov <- vnames$ov.ind[[1]]
  if (length(lv) == 0) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "The model contains no latent variables"
    )
    return(out)
  }
  # build output
  n.ind <- sapply(lv, function (var) {
    sum(
      with(partable, var == lhs & any(ov %in% rhs) & op == "=~")
    )
  }, simplify = TRUE)
  n.cind <- sapply(lv, function (var) {
    sum(
      with(partable, var == lhs & any(ov %in% rhs) & op == "~")
    )
  }, simplify = TRUE)
  if (all(n.cind == 0)) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "This rule only applies when causal indicators are present"
    )
    return(out)
  }
  if (length(lv) == 1) {
    rule <- paste(rule, "(1 LV)")
    pass <- isTRUE(n.ind >= 2)
    cond <- "S"
    warn <- ifelse(
      pass, NA,
      "The model must have more than one effect indicator"
      )
  } else {
    # TODO: finish multiple LV MIMIC rule
    pass <- NA
    cond <- NA
    warn <- NA
  }
  out <- list(
    rule,
    pass,
    cond,
    warn
  )
  return(out)
}
