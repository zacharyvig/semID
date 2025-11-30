#' Evaluate common Structural Equation Model (SEM) identification rules
#'
#' ...
#'
#' @param model A character string model in \code{lavaan} syntax or a
#'  \code{lavaan} parameter table.
#' @param warn Logical. If \code{TRUE}, the output will include why a rule does
#'  not pass.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'  \code{lavaan}. See \link[lavaan]{lavaanify} for more information.
#'
#' @return Nothing. Returns once summary table is printed.
#'
#' @importFrom lavaan lavaanify
#'
#' @examples
#' # The Holzinger and Swineford (1939) example
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' id(HS.model, warn = TRUE, meanstructure = FALSE)
#' @export
id <- function(model = NULL, warn = TRUE, ...) {
  # STEP 0 - Parse input
  dotdotdot <- list(...)

  dotdotdot$model <- model
  dotdotdot$warn <- TRUE
  dotdotdot$debug <- FALSE

  partable <- do.call(lavaanify, dotdotdot)

  # STEP 1 - Classify model
  mod_type <- classify_model(partable)

  if (is.na(mod_type) | mod_type == "mlm") {
    stop("This model type is not currently supported")
  }

  rules <- c(
    # STEP 2 - Evaluate general rules
    lapply(
      .any_rules, function(rule) do.call(rule, list(partable))
    ),
    # STEP 3 - Evaluate model type-specific rules
    lapply(
      get(paste0(".", mod_type, "_rules")),
      function(rule) do.call(rule, list(partable))
    )
  )

  # STEP 4 - Print Rules (and Warnings)
  wind <- 56L
  names <- c(
    format("", width = wind - 34L),
    "Pass", "Necessary", "Sufficient"
  )
  widx <- 0 # warning index
  if (warn) {
    names[5] <- "Warning"
    wrns <- c()
  }
  cat(names, "\n\n")
  for (i in 1:length(rules)) {
    row <- c(
      # rule title
      format(rules[[i]]$rule, width = wind - 34L),
      # pass?
      format(
        switch(as.character(rules[[i]]$pass),
               "NA" = "NA",
               "TRUE" = "Yes",
               "FALSE" = "No"),
        width = 4L, justify = "right"
      ),
      # necessary and/or sufficient?
      switch(as.character(rules[[i]]$cond),
        "N" = c("Yes", "No"),
        "S" = c("No", "Yes"),
        "NS" = c("Yes", "Yes"),
        "NA" = rep("-", 2)
      )
    )
    row[3:4] <- c(
      format(row[3], width = 9L, justify = "right"),
      format(row[4], width = 10L, justify = "right")
    )
    if (warn & all(!is.na(rules[[i]]$warn))) {
      idx.w0 <- c()
      for (wrn in rules[[i]]$warn) {
        if (wrn %in% wrns) {
          idx.w0 <- c(idx.w0, which(wrns == wrn))
        } else {
          widx <- widx + 1
          idx.w0 <- c(idx.w0, widx)
          wrns <- c(wrns, wrn)
        }
      }
      cat(
        c(row, format(
          paste(idx.w0, collapse = ","),
          width = 7L, justify = "right")),
        "\n"
        )
    } else {
      cat(row, "\n")
    }
  }
  if (warn & widx > 0) {
    cat("\nWarnings:\n")
    for (i in 1:widx) {
      w0 <- strwrap(wrns[i], width = wind - 4L)
      ls <- length(w0)
      if (ls > 1L) {
        w0 <- paste0(c(sprintf("%s - ", i), rep(strrep(" ", 4L), ls - 1L)), w0)
      }
      cat(w0, sep = "\n")
    }
  }
  return(invisible(rules))
}
