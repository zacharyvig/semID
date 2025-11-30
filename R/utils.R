# expand.grid alternative that returns a list
# not used for now
combo.list <- function(x, y = NULL, skip_repeats = FALSE) {
  if (is.null(y)) y <- x
  nx <- length(x)
  len <- ifelse(skip_repeats, choose(nx, 2), nx * nx)
  out <- as.list(rep(NA, len))
  s <- 1
  for (i in x) {
    for (j in y) {
      if (skip_repeats & i == j) next
      out[[s]] <- c(i, j)
      s <- s + 1
    }
  }
  return(out)
}
