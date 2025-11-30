#' Function to check if a simultaneous equations model is recursive
#' @keywords internal
check_recursion <- function(partable, base, comp) {
  if (any(comp %in% base)) {
    # end on feedback loop
    return(FALSE)
  } else {
    # new comparison variables
    newcomp <- sapply(comp, function(var) {
      with(partable, lhs[rhs == var & op == "~"])
    }, simplify = TRUE, USE.NAMES = FALSE)
    if (length(unlist(newcomp)) > 0) {
      check_recursion(partable, c(base, comp), unlist(newcomp))
    } else {
      # end on exogenous variables
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
#'  rules. Sub-types include "reg" (simultaneous equation models/regression models) and
#'  "cfa" (confirmatory factor analysis models). Use "sem" to get rules that apply
#'  to all structural equation models. Use "*" to get all rules in the package.
#'
#' @return A list object with the rule function(s) specified by the user.
#'
#' @export
#' @author Zach Vig
get_rules <- function(rule = "*", model_type = "*") {
  stopifnot(
    "Unknown `model_type`" = model_type %in% c("*", "reg", "cfa", "sem")
  )
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
