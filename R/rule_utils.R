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
#' The `semID` package stores identification rule functions internally. This
#' function makes them available to the user as a list.
#'
#' @param rule A string specifying the name of the rule as it's defined
#'  in the package. Use "*" to get all rules (or all rules of the defined
#'  model type). Partial matches are acceptable.
#' @param model_type A string specifying the model sub-type from which to get
#'  rules. Sub-types include "reg" (simultaneous equations models/regression
#'  models) and "cfa" (confirmatory factor analysis models). Use "sem" to get
#'  rules that apply to all structural equation models. Use "*" to get all rules
#'  in the package.
#'
#' @return A list object with the rule function(s) specified by the user.
#'
#' @export
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
  pull_fns <- function(fns) {
    mget(fns, env = asNamespace("semID"), mode = "function")
  }
  if (model_type == "*") model_type <- "all"
  rules <- get_rule_names(model_type)
  if (rule %in% c("*", "all")) {
    return(pull_fns(rules))
  } else {
    rule <- grep(rule, rules, value = TRUE)
    stopifnot(
      "Specified rule does not exist" =
        isTRUE(rule %in% rules)
    )
    return(pull_fns(rule))
  }
}

#' internal function for extracting rule function names
#' @noRd
get_rule_names <- function(model_type = c("all", "reg", "cfa", "sem")) {
  model_type <- match.arg(model_type)
  prefix <- if (model_type == "all") {
    "^rule_"
  } else {
    sprintf("^rule_%s", model_type)
  }
  ns <- asNamespace("semID")
  objs <- ls(envir = ns, all.names = TRUE)
  grep(prefix, objs, value = TRUE)
}

#' internal function for building rule output lists
#' @noRd
build_rule_out <- function(rule, pass, msgs = NA, cond = c("N", "S", "NS", NA)) {
  cond <- match.arg(cond)
  msgs <- if (pass || !is.na(msgs)) msgs else NA
  out <- list(
    rule = rule,
    pass = pass,
    msgs = msgs,
    cond = cond
  )
  return(out)
}

#' internal function to get certain variables from a lavaan parameter table
#' @noRd
get_partable_vars <- function(partable, vars, var_names = NA) {
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # tally variables assuming one block
  out <- lapply(vars, function(var) {
    out <- vnames[[var]][[1]]
    return(out)
  })
  if (!all(is.na(var_names))) {
    names(out) <- var_names
  } else {
    names(out) <- vars
  }
  return(out)
}