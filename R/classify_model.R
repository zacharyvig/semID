#' Classify a model using the parameter table for internal use
#' @importFrom lavaan lav_partable_attributes
#' @keywords internal
#' @author Zach Vig
classify_model <- function(partable = NULL) {
  model_type <- NA
  # retrieve attributes and variable names
  lavpta <- lav_partable_attributes(partable)
  vnames <- lavpta$vnames
  # check for MLM
  if (lavpta$nblocks > 1) {
    model_type <- "mlm"
    return(model_type)
  }
  # tally variables
  nlv <- lapply(vnames$lv, length)
  nov.nox <- lapply(vnames$ov.nox, length)
  neqs.y <- lapply(vnames$eqs.y, length)
  # check a list for counts greater than zero
  nonzero <- function(x) {
    return(isTRUE(x > 0))
  }
  # classify model
  if (any(sapply(nlv, nonzero))) {
    if (any(sapply(neqs.y, nonzero))) {
      # lvs present and regressions present
      model_type <- "sem"
    } else {
      # lvs present but no regressions
      model_type <- "cfa"
    }
  } else {
    if (any(sapply(nov.nox, nonzero))) {
      # no lvs but regressions present
      model_type <- "reg"
    }
  }
  return(model_type)
}
