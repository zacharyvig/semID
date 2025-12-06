#' Function to check if a simultaneous equations model is recursive
#' @keywords internal
check_recursion <- function(partable, base, comp, n.paths) {
  if (any(comp %in% base)) {
    # end on feedback loop
    return(FALSE)
  } else {
    # new comparison variables
    newcomp <- sapply(comp, function(var) {
      with(partable, lhs[rhs == var & op == "~"])
    }, simplify = TRUE, USE.NAMES = FALSE)
    n <- n.paths - length(unlist(newcomp))
    if (length(unlist(newcomp)) > 0 & n > 0) {
        out <- check_recursion(
          partable = partable,
          base = base,
          comp = unlist(newcomp),
          n.paths = n
          )
        return(out)
    } else {
      # end on exogenous variables or use of all paths
      return(TRUE)
    }
  }
}

#' Gather identification rules as a list
#'
#' The `semID` package stores identification rule functions internally. This function
#'  makes them available to the user as a list.
#'
#' @param rule A string specifying the name of the rule as it's defined
#'  in the package. Use "*" to get all rules (or all rules of the defined
#'  model type)
#' @param model_type A string specifying the model sub-type from which to get
#'  rules. Sub-types include "reg" (simultaneous equations models/regression models) and
#'  "cfa" (confirmatory factor analysis models). Use "sem" to get rules that apply
#'  to all structural equation models. Use "*" to get all rules in the package.
#'
#' @return A list object with the rule function(s) specified by the user.
#'
#' @export
#' @author Zach Vig
#'
#' @examples
#' \dontrun{
#'   rules <- get_rules(rule = "*", model_type = "cfa")
#' }
#'
get_rules <- function(rule = "*", model_type = "*") {
  stopifnot(
    "Unknown `model_type`" = model_type %in% c("*", "reg", "cfa", "sem")
  )
  if (rule == "all" ) rule == "*"
  if (model_type == "all") rule == "all"
  if (model_type == "*") {
    rules <- c(
      .sem_rules,
      .reg_rules,
      .cfa_rules
    )
  } else {
    rules <- as.list(get(sprintf(".%s_rules", model_type)))
  }
  if (rule == "*") {
    return(rules)
  } else {
    stopifnot(
      "Specified rule does not exist (or check `model_type`)" = !is.null(rules[[rule]])
    )
    return(rules[[rule]])
  }
}


#' Check if latent variables are correctly scaled
#'
#' This is a helper function for checking whether all latent variables in a model
#' are scaled.
#'
#' @importFrom lavaan lav_partable_npar lav_partable_ndat
#'
#' @param partable A \code{lavaan} parameter table
#' @param lv An optional character vector of the specific latent variables you
#' would like to check. Otherwise, all latent variables are extracted from the
#' parameter table
#'
#' @importFrom lavaan lav_partable_npar lav_partable_ndat
#'
#' @return An (invisible) logical vector specifying whether each latent variable
#' (included as the names of the vector) is correctly scaled
#'
#' @export
#' @author Zach Vig
#'
#' @examples
#' \dontrun{
#' mod1 <- ' l1 =~ x1 + x2
#'           l2 =~ x3 + x4 + x5 '
#' mod1.partable <- lavaan::lavaanify(m1, auto = TRUE, model.type = "sem")
#' mod1.scaled <- scaled(m1.partable, lv = "l2")
#' }
#'
scaled <- function(partable, lv = NULL) {
  meanstructure <- any(partable$op == "~1")
  if (is.null(lv)) {
    # retrieve attributes and variable names
    lavpta <- lav_partable_attributes(partable)
    vnames <- lavpta$vnames
    # tally variables assuming one block
    lv <- vnames$lv[[1]]
  }
  if (length(lv) == 0) {
    return(invisible(NA))
  }
  vec <- vector(length = length(lv))
  names(vec) <- lv
  for (var in lv) {
    # one-indicator case
    one.ind <- sum(with(partable, lhs == var & op == "=~" & rhs != var)) == 1
    if (one.ind) {
      vec[var] <- FALSE
      break
    }
    # two-indicator case / no lv correlations
    two.ind <- sum(with(partable, lhs == var & op == "=~")) == 2
    cov.lv <- any(with(partable, (lhs == var & rhs != var) |
                         (rhs == var & lhs != var) & op == "~~"))
    if (two.ind & !cov.lv) {
      check0 <- sum(
        with(partable, lhs == var & op == "=~" & free == 0)
        ) == 2
      vec[var] <- check0
      break
    } else {
      check0 <- TRUE
    }
    # scaling indicator?
    scale.ind.idx <- with(partable, lhs == var & op == "=~" &
                            free == 0 & rhs != var)
    if (any(scale.ind.idx)) {
      check1 <- any(scale.ind.idx)
      scale.ind <- with(partable, rhs[scale.ind.idx])
      # scaling indicator mean fixed?
      check2 <- ifelse(
        meanstructure,
        any(with(partable, lhs %in% scale.ind & op == "~1" & free == 0)),
        TRUE
      )
    } else {
      check1 <- check2 <- FALSE
    }
    # latent variance fixed?
    check3 <- any(
      with(partable, lhs == var & rhs == var & op == "~~" & free == 0)
    )
    # latent mean fixed?
    check4 <- ifelse(
      meanstructure,
      with(partable, lhs == var & op == "~1" & free == 0),
      TRUE
    )
    vec[var] <- isTRUE(check1 & check2) |
      isTRUE(check3 & check4) | isTRUE(check1 & check3)
  }
  return(invisible(vec))
}
