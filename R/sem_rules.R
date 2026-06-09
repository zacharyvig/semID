#' Rules for all structural equation models
#' @name sem_rules
#' @keywords internal
NULL


# N_theta rule (compares parameters to observed statistics)
#' @rdname sem_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
rule_sem_ntheta <- function(partable) {
  # number of parameters
  npar <- lav_partable_npar(partable)
  # number of means, variances, and covariances
  ndat <- lav_partable_ndat(partable)
  # build output
  rule <- "N_theta Rule"
  pass <- isTRUE(npar <= ndat)
  msgs <- ifelse(
    pass, NA,
    sprintf("[WARNING] The number of free parameters (x%s) exceeds the number of means/variances/covariances (x%s)", npar, ndat)
  )
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "N"
  )
  return(out)
}

# Latent Scaling rule
#' @rdname sem_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen, K. A., Lilly, A. G., & Luo, L. (2024). Selecting scaling
#' indicators in structural equation models (SEMs).
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
rule_sem_latent_scaling <- function(partable) {
  rule <- "Latent Scaling Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, "lv")
  if (length(vars$lv) == 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = "[Info] This rule only applies when there are latent variables in the model",
      cond = NA_character_
    )
    return(out)
  }
  # build output
  scaled <- scaling(partable, lv = vars$lv)
  pass <- isTRUE(all(scaled))
  cond <- "N"
  if (!pass) {
    msgs <- paste("[WARNING] Some latent variables are not scaled:",
              paste(vars$lv[!scaled], collapse = ", "))
  } else {
    msgs <- NA
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
  return(out)
}

# 2+ Emitted Paths rule
#' @rdname sem_rules
#'
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Bollen & Davis (2009). Two Rules of Identification for Structural Equation Models.
#' @keywords internal
rule_sem_two_emitted_paths <- function(partable) {
  rule <- "2+ Emitted Paths Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, "lv")
  if (length(vars$lv) == 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = "[Info] This rule only applies when there are latent variables in the model",
      cond = NA_character_
    )
    return(out)
  }
  # build output
  free_var <- sapply(vars$lv, function(var) {
    c1 <- isTRUE(
      with(partable, free[lhs == var & rhs == var & op == "~~"]) > 0
    )
    return(c1)
  })
  free_var.nox <- sapply(vars$lv, function(var) {
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
  two_path.lv <- sapply(vars$lv, function(var) {
    c3 <- sum(
      with(
        partable,
        lhs == var & op == "=~" & rhs != var | rhs == var & op == "~"
      )
    )
    return(c3 >= 2)
  })
  if (all(!free_var | !free_var.nox)) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = "[Info] There are no variables (with free variance & free downstream disturbance variances) to which to apply the rule",
      cond = NA
    )
    return(out)
  } else {
    pass <- isTRUE(all(two_path.lv[free_var & free_var.nox]))
    cond <- "N"
  }
  # messages
  msgs <- c()
  if (!pass) {
    msgs <- c(
      msgs,
      paste(
        "[WARNING] Some variables do not have two emitted paths:",
        paste(vars$lv[free_var & free_var.nox & !two_path.lv], collapse = ", ")
      )
    )
  }
  if (any(!free_var)) {
    msgs <- c(
      msgs,
      paste(
        "[Info] This rule ignores latent variables without free variance:",
        paste(vars$lv[!free_var], collapse = ", ")
      )
    )
  }
  if (any(!free_var.nox)) {
    msgs <- c(
      msgs,
      paste(
        "[Info] This rule ignores latent variables with downstream variables that do not have free disturbance variances:",
        paste(vars$lv[!free_var.nox], collapse = ", ")
      )
    )
  }
  if (length(msgs) == 0) msgs <- NA
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
  return(out)
}

# Exogenous X rule/MIMIC rules
#' @rdname sem_rules
#'
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Bollen & Davis (2009). Two Rules of Identification for Structural Equation Models.
#' @keywords internal
rule_sem_exogenous_x <- function(partable) {
  rule <- "Exogenous X Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov"))
  if (length(vars$lv) == 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = "[Info] This rule only applies when there are latent variables in the model",
      cond = NA_character_
    )
    return(out)
  }
  # build output
  n.ind <- sapply(vars$lv, function (var) {
    sum(
      with(partable, lhs == var & any(vars$ov %in% rhs) & op == "=~")
    )
  }, simplify = TRUE)
  n.cind <- sapply(vars$lv, function (var) {
    sum(
      with(partable, lhs == var & any(vars$ov %in% rhs) & (op == "~" | op == "<~"))
    )
  }, simplify = TRUE)
  if (all(n.cind == 0)) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      msgs = "[Info] This rule only applies when causal indicators are in the model",
      cond = NA_character_
    )
    return(out)
  }
  if (length(vars$lv) == 1) {
    pass <- isTRUE(n.ind >= 2)
    cond <- "S"
    msgs <- ifelse(
      pass, NA,
      "[Fail] The model must have more than one effect indicator"
      )
  } else {
    # TODO: finish multiple LV MIMIC rule
    pass <- cond <- NA
    msgs <- "[Info] This rule is currently not supported for multiple latent variables"
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
  return(out)
}
