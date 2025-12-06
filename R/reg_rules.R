# Environment to store simultaneous equations model/regression rules
.reg_rules <- list()

#' Null B_YY Rule
#' @name null_byy_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @importFrom lavaan lav_partable_attributes
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#'
#' @keywords internal
#' @author Zach Vig
.reg_rules$null_byy_rule <- function(partable) {
  rule <- "Null B_YY Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # extract variables assuming one block
  lv <- vnames$lv[[1]]
  ov <- vnames$ov[[1]]
  ov.ox <- intersect(vnames$eqs.x[[1]], ov)
  ov.nox <- intersect(vnames$eqs.y[[1]], ov)
  if (length(lv) > 0) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "This rule only applies when there are no latent variables in the model"
    )
    return(out)
  }
  # check if endogenous vars appear as exogenous vars
  nox_ox <- ov.nox %in% ov.ox; names(nox_ox) <- ov.nox
  # build output
  if (any(nox_ox[!is.na(nox_ox)])) {
    pass <- FALSE
    warn <- paste(
      "One or more endogenous variables appear as regression predictors:",
      paste(ov.nox[nox_ox], collapse = ", ")
    )
  } else {
    pass <- TRUE
    warn <- NA
  }
  out <- list(
    rule = rule,
    pass = pass,
    warn = warn,
    cond = "S"
  )
  return(out)
}

#' Fully Recursive model rule
#' @name fully_recursive_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @importFrom lavaan lav_partable_attributes
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
#' @author Zach Vig
.reg_rules$fully_recursive_rule <- function(partable) {
  rule <- "Fully Recursive Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # extract endogenous variables assuming one block
  lv <- vnames$lv[[1]]
  ov <- vnames$ov[[1]]
  ov.nox <- vnames$eqs.y[[1]]
  if (length(lv) > 0) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "This rule only applies when there are no latent variables in the model"
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
    warn <- "Feedback loops exist in the model, i.e., the model is non-recursive"
  } else if (any(cor_err.ov.nox)) {
    pass <- FALSE
    viol <- paste(c(covs$lhs[cor_err.ov.nox], covs$rhs[cor_err.ov.nox]), sep = "/")
    warn <- paste(
      "The model is recursive but some endogenous variables have correlated errors:",
      paste(viol, collapse = ", ")
    )
  } else {
    pass <- TRUE
    warn <- NA
  }
  out <- list(
    rule = rule,
    pass = pass,
    warn = warn,
    cond = "S"
  )
  return(out)
}

#' Recursive model with correlated errors rule
#' @name recursive_corr_err_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @importFrom lavaan lav_partable_attributes
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @references Brito, C., & Pearl, J. (2002). A new identification condition
#' for recursive models with correlated errors.
#'
#' @keywords internal
#' @author Zach Vig
.reg_rules$recursive_corr_err_rule <- function(partable) {
  rule <- "Recur/Corr Err Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # extract endogenous variables assuming one block
  lv <- vnames$lv[[1]]
  ov <- vnames$ov[[1]]
  ov.nox <- intersect(vnames$eqs.y[[1]], ov)
  if (length(lv) > 0) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "This rule only applies when there are no latent variables in the model"
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
    warn <- "Feedback loops exist in the model, i.e., the model is non-recursive"
  } else if (any(cor_err.eqs)) {
    pass <- FALSE
    viol <- paste(covs$lhs[cor_err.eqs], covs$rhs[cor_err.eqs], sep = "/")
    warn <- paste(
      "The model is recursive but some directly related variables have correlated errors:",
      paste(viol, collapse = ", ")
    )
  } else {
    pass <- TRUE
    warn <- NA
  }
  out <- list(
    rule = rule,
    pass = pass,
    warn = warn,
    cond = "S"
  )
  return(out)
}
