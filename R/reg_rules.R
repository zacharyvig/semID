#' Rules for simultaneous equation/regression models
#' @name reg_rules
#' @keywords internal
NULL

# Null B_YY Rule
#' @rdname reg_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#'
#' @keywords internal
rule_reg_null_byy <- function(partable) {
  rule <- "Null B_YY Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov", "eqs.x", "eqs.y"))
  ov.ox <- intersect(vars$eqs.x, vars$ov)
  ov.nox <- intersect(vars$eqs.y, vars$ov)
  if (length(vars$lv) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      cond = NA_character_,
      msgs = "[Info] This rule only applies when there are no latent variables in the model"
    )
    return(out)
  }
  # check if endogenous vars appear as exogenous vars
  nox_ox <- ov.nox %in% ov.ox; names(nox_ox) <- ov.nox
  # build output
  if (any(nox_ox[!is.na(nox_ox)])) {
    pass <- FALSE
    msgs <- paste(
      "[Fail] One or more endogenous variables appear as regression predictors:",
      paste(ov.nox[nox_ox], collapse = ", ")
    )
  } else {
    pass <- TRUE
    msgs <- NA
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "S"
  )
  return(out)
}

# Fully Recursive model rule
#' @rdname reg_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
rule_reg_fully_recursive <- function(partable) {
  rule <- "Fully Recursive Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov", "eqs.y"))
  ov.nox <- intersect(vars$eqs.y, vars$ov)
  if (length(vars$lv) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      cond = NA_character_,
      msgs = "[Info] This rule only applies when there are no latent variables in the model"
    )
    return(out)
  }
  # check recursion
  for (var in ov.nox) {
    n.paths <- sum(with(partable, op == "~" & rhs %in% ov.nox))
    comp <- with(partable, lhs[rhs == var & op == "~"])
    recursive <- check_recursion(
      partable = partable, base = var,
      comp = comp,
      n.paths = n.paths - length(comp)
    )
    if (!recursive) break
  }
  # check uncorrelated errors of endogenous variables
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.ov.nox <- with(covs, lhs %in% ov.nox & rhs %in% ov.nox)
  # build output
  if (isFALSE(recursive)) {
    pass <- FALSE
    msgs <- "[Fail] Feedback loops exist in the model, i.e., the model is non-recursive"
  } else if (any(cor_err.ov.nox)) {
    pass <- FALSE
    viol <- paste(c(covs$lhs[cor_err.ov.nox], covs$rhs[cor_err.ov.nox]), sep = "/")
    msgs <- paste(
      "[Fail] The model is recursive but some endogenous variables have correlated errors:",
      paste(viol, collapse = ", ")
    )
  } else {
    pass <- TRUE
    msgs <- NA
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "S"
  )
  return(out)
}

# Recursive model with correlated errors rule
#' @rdname reg_rules
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Brito, C., & Pearl, J. (2002). A new identification condition
#' for recursive models with correlated errors.
#'
#' @keywords internal
rule_reg_recursive_corr_err <- function(partable) {
  rule <- "Recur/Corr Err Rule"
  # retrieve attributes and variable names
  vars <- get_partable_vars(partable, c("lv", "ov", "eqs.y"))
  ov.nox <- intersect(vars$eqs.y, vars$ov)
  if (length(vars$lv) > 0) {
    out <- build_rule_out(
      rule = rule,
      pass = NA,
      cond = NA_character_,
      msgs = "[Info] This rule only applies when there are no latent variables in the model"
    )
    return(out)
  }
  # check recursion
  for (var in ov.nox) {
    n.paths <- sum(with(partable, op == "~"))
    comp <- with(partable, lhs[rhs == var & op == "~"])
    recursive <- check_recursion(
      partable = partable, base = var,
      comp = comp,
      n.paths = n.paths - length(comp)
    )
    if (!recursive) break
  }
  # check uncorrelated errors of regression variables
  regs <- subset(partable, op == "~" & free > 0)
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.eqs <- unname(
    apply(covs, 1, function(row) {
      any(with(regs, lhs %in% row["lhs"] & rhs %in% row["rhs"] |
                 lhs %in% row["rhs"] & rhs %in% row["lhs"]))
    }, simplify = TRUE)
  )
  # build output
  if (isFALSE(recursive)) {
    pass <- FALSE
    msgs <- "[Fail] Feedback loops exist in the model, i.e., the model is non-recursive"
  } else if (any(cor_err.eqs)) {
    pass <- FALSE
    viol <- paste(covs$lhs[cor_err.eqs], covs$rhs[cor_err.eqs], sep = "/")
    msgs <- paste(
      "[Fail] The model is recursive but some directly related variables have correlated errors:",
      paste(viol, collapse = ", ")
    )
  } else {
    pass <- TRUE
    msgs <- NA
  }
  out <- build_rule_out(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = "S"
  )
  return(out)
}
