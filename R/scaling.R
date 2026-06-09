#' Check if latent variables are correctly scaled
#'
#' This is a helper function for checking whether all latent variables in a model
#' are scaled.
#'
#' @param partable A \code{lavaan} parameter table
#' @param lv An optional character vector of the specific latent variables you
#' would like to check. Otherwise, all latent variables are extracted from the
#' parameter table
#'
#' @return An (invisible) logical vector specifying whether each latent variable
#' (included as the names of the vector) is correctly scaled
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mod1 <- ' l1 =~ x1 + x2
#'           l2 =~ x3 + x4 + x5 '
#' mod1.partable <- lavaan::lavaanify(mod1, auto = TRUE, model.type = "sem")
#' mod1.scaled <- scaling(mod1.partable, lv = "l2")
#' }
#'
scaling <- function(partable, lv = NULL) {
  meanstructure <- any(partable$op == "~1")
  if (is.null(lv)) {
    # retrieve attributes and variable names
    vars <- get_partable_vars(partable, "lv")
    lv <- vars$lv
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
    if (two.ind && !cov.lv) {
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
    check1 <- check2 <- any(scale.ind.idx)                            
    if (check1) {
      scale.ind <- with(partable, rhs[scale.ind.idx])
      # scaling indicator mean fixed?
      check2 <- ifelse(
        meanstructure,
        any(with(partable, lhs %in% scale.ind & op == "~1" & free == 0)),
        TRUE
      )
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
    vec[var] <- isTRUE(check1 && check2) |
      isTRUE(check3 && check4) | isTRUE(check1 && check3)
  }
  return(invisible(vec))
}
