# Environment to store confirmatory factor analysis model rules
.cfa_rules <- list()

#' Three Indicator rules
#' @name three_indicator_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
#' @author Zach Vig
.cfa_rules$three_indicator_rule <- function(partable) {
  rule <- "Three Indicator Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # tally variables assuming one block
  lv <- vnames$lv[[1]]
  ov <- vnames$ov.ind[[1]]
  # tally indicators per lv
  nov.ind <- sapply(lv, function(var){
    out <- with(partable, rhs[lhs == var & op == "=~" & rhs %in% ov])
    return(length(out))
  }, simplify = TRUE)
  # check for higher order cfa
  nlv.ind <- sapply(lv, function(var){
    out <- with(partable, rhs[lhs == var & op == "=~" & rhs %in% lv])
    return(length(out))
  }, simplify = TRUE)
  # build output
  if(any(nov.ind[nlv.ind == 0] < 3)) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = paste("Some first order factors have less than three indicators:",
              paste(lv[nov.ind < 3 & nlv.ind == 0], collapse = ", "))
    )
    return(out)
  }
  # first order factors
  idx.fof <- which(nlv.ind == 0)
  ind.fof <- with(partable, rhs[lhs %in% lv[idx.fof] & op == "=~"])
  # factor complexity 1
  fc1.fof <- sapply(ind.fof, function(ind) {
    length(
      with(partable, lhs[rhs == ind & op == "=~"])
    ) == 1
  }, simplify = TRUE)
  # uncorrelated errors
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.ind <- sapply(ind.fof, function(ind) {
    with(covs, any(lhs == ind & rhs %in% ind.fof | rhs == ind & lhs %in% ind.fof))
  }, simplify = TRUE)
  # warnings
  warn <- c()
  if (any(nlv.ind > 0)) {
    cond <- NA
    warn <- c(warn, "Cannot establish sufficiency when higher order factors are present; pass/fail applies to first order factors only")
  } else {
    cond <- "S"
  }
  if (any(!fc1.fof)) {
    pass <- FALSE
    warn <- c(warn, paste("Some indicators have a factor complexity greater than one:",
              paste(ind.fof[!fc1.fof], collapse = ", ")))
  } else if (any(cor_err.ind)) {
    pass <- FALSE
    warn <- c(warn, paste("Some indicators have correlated errors:",
              paste(ind.fof[cor_err.ind], collapse = ", ")))
  } else {
    pass <- TRUE
  }
  if(length(warn) == 0) warn <- NA
  out <- list(
    rule = rule,
    pass = pass,
    warn = warn,
    cond = cond
  )
  return(out)
}


#' Two Indicator rules
#' @name two_indicator_rule
#' @param partable A \code{lavaan} parameter table
#'
#' @references Bollen (2026). Elements of Structural Equation Models (SEMs).
#' @keywords internal
#' @author Zach Vig
.cfa_rules$two_indicator_rule <- function(partable) {
  rule <- "Two Indicator Rule"
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # tally variables assuming one block
  lv <- vnames$lv[[1]]
  ov <- vnames$ov.ind[[1]]
  if (length(lv) < 2) {
    out <- list(
      rule = rule,
      pass = NA,
      cond = NA,
      warn = "The model contains less than two latent variables"
    )
    return(out)
  }
  # tally indicators per lv
  nov.ind <- sapply(lv, function(var){
    out <- with(partable, rhs[lhs == var & op == "=~" & rhs %in% ov])
    return(length(out))
  }, simplify = TRUE)
  # check for higher order cfa
  nlv.ind <- sapply(lv, function(var){
    out <- with(partable, rhs[lhs == var & op == "=~" & rhs %in% lv])
    return(length(out))
  }, simplify = TRUE)
  # build output
  if(any(nov.ind[nlv.ind == 0] < 2)) {
    out <- list(
      rule = rule,
      pass = NA,
      warn = paste("Some first order factors have less than two indicators:",
                   paste(lv[nov.ind < 2 & nlv.ind == 0], collapse = ", ")),
      cond = NA
    )
    return(out)
  }
  # first order factors
  idx.fof <- which(nlv.ind == 0)
  ind.fof <- with(partable, rhs[lhs %in% lv[idx.fof] & op == "=~"])
  # factor complexity 1
  fc1.fof <- sapply(ind.fof, function(ind) {
    length(with(partable, lhs[rhs == ind & op == "=~"])) == 1
  }, simplify = TRUE)
  # uncorrelated errors
  covs <- subset(partable, op == "~~" & lhs != rhs & free > 0)
  cor_err.ind <- sapply(ind.fof, function(ind) {
    with(covs, any(lhs == ind & rhs %in% ind.fof | rhs == ind & lhs %in% ind.fof))
  }, simplify = TRUE)
  # lv variances/correlations
  cor.lv <- sapply(lv[idx.fof], function(var) {
    with(covs, any(lhs == var & rhs %in% lv | rhs == var & lhs %in% lv))
  }, simplify = TRUE)
  # warnings
  warn <- c()
  if (any(nlv.ind > 0)) {
    cond <- NA
    warn <- c(warn, "Cannot establish sufficiency when higher order factors are present; pass/fail applies to first order factors only")
  } else {
    cond <- "S"
  }
  if (any(!fc1.fof)) {
    pass <- FALSE; viol <- ind.fof[!fc1.fof]
    warn <- c(warn, paste("Some indicators have a factor complexity greater than one:",
                          paste(viol, collapse = ", ")))
  } else if (any(cor_err.ind)) {
    pass <- FALSE; viol <- ind.fof[cor_err.ind]
    warn <- c(warn, paste("Some indicators have correlated errors:",
                          paste(viol, collapse = ", ")))
  } else if (any(!cor.lv)) {
    pass <- FALSE
    warn <- c(warn, paste("Some latent variables are not correlated with another latent variable:",
              paste(lv[idx.fof & !cor.lv] , collapse = ", ")))
  } else {
    pass <- TRUE
  }
  if(length(warn) == 0) warn <- NA
  out <- list(
    rule = rule,
    pass = pass,
    warn = warn,
    cond = cond
  )
  return(out)
}
