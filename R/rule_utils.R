#' Check recursion using Depth-First Search algorithm
#' @param partable A lavaan parameter table
#' @param x A character vector of variable names from which to start the algorithm.
#' @keywords internal
check_recursion <- function(partable, x) {
  # regressions
  regs <- subset(partable, op == "~" & free > 0)
  ds_full <- split(regs$lhs, regs$rhs)

  loop <- function(x, tally) {
    tally <- c(tally, x)
    ds <- ds_full[[x]]
    if (length(ds)) {
      for (var in ds) {
        if (var %in% tally) {
          return(FALSE)
        } else {
          out <- loop(var, tally)
          if (isFALSE(out)) {
            return(FALSE)
          }
        }
      }
    }
    return(TRUE)
  }

  for (var in x) {
    out <- loop(var, tally = c())
    if (isFALSE(out)) {
      return(FALSE)
    }
  }

  return(TRUE)
  
}

#' Convert a SEM into a Confirmatory Factor Analysis for the two-step rule
#' @param partable A lavaan parameter table
#' @noRd
sem_to_cfa <- function(partable) {
  vars <- get_partable_vars(partable, c("lv"))
  lv.regs <- with(partable, op == "~" & lhs %in% vars$lv & rhs %in% vars$lv)
  # replace directional arrows with double-sided ones
  partable$op[lv.regs] <- "~~"
  return(partable)
}

#' Convert a SEM into a Simultaneous Equations Model for the two-step rule
#' @param partable A lavaan parameter table
#' @noRd
sem_to_reg <- function(partable) {
  vars <- get_partable_vars(partable, c("lv"))
  lv.paths <- with(partable, lhs %in% vars$lv & rhs %in% vars$lv)
  partable <- partable[lv.paths, ]
  # handle higher order factors
  partable$op[partable$op == "=~"] <- "~"
  # handle causal indicators
  partable$op[partable$op == "<~"] <- "~"
  return(partable)
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
    mget(fns, envir = asNamespace("semID"), mode = "function")
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
build_rule_out <- function(rule, pass, msgs = NA_character_, cond = c("N", "S", "NS", NA_character_)) {
  cond <- match.arg(cond)
  msgs <- if (pass || !is.na(msgs)) msgs else NA_character_
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
  lavpta <- lavaan::lav_partable_attributes(partable)
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

#' Internal function for adding messages to rule output
#' @noRd
add_rule_msgs <- function(msgs = NA_character_, new_msgs, levels = NULL) {
  if (is.na(msgs)) msgs <- c()
  stopifnot(length(new_msgs) == length(levels))
  level.labels <- c(
    "1" = "Info",
    "2" = "Reason",
    "3" = "WARNING"
  )
  if (is.null(levels)) levels <- rep(NA, length(new_msgs))
  new_msgs <- ifelse(
    is.na(levels),
    new_msgs,
    sprintf("[%s] %s", level.labels[as.character(levels)], new_msgs)
  )
  out <- c(msgs[!is.na(msgs)], new_msgs)
  return(out)
}
  