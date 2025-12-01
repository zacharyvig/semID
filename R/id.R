#' Evaluate common Structural Equation Model (SEM) identification rules
#'
#' ...
#'
#' @param model A character string model in \code{lavaan} syntax, a
#'  \code{lavaan} parameter table, or a fitted \code{lavaan} object.
#' @param warn Logical. If \code{TRUE}, the output will include why a rule does
#'  not pass or is not applicable, along with any other helpful information.
#' @param call A character string specifying the call you intend to use to fit
#'  the model. This will ensure the correct model defaults are specified. Options
#'  currently include "lavaan", "sem", or "cfa".
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
id <- function(model = NULL, warn = TRUE, call = "sem", ...) {
  # Checks
  stopifnot(
    "Argument `warn` must be a logical" =
      is.logical(warn)
  )
  stopifnot(
    "Unknown `call` or `call` currently not supported" =
      call %in% c("lavaan", "sem", "cfa")
  )

  # STEP 0 - Parse input
  if (inherits(model, "lavaan")) {
    partable <- as.data.frame(model@ParTable, stringsAsFactors = FALSE)
  # check if already partable (from lav_partable)
  } else if (is.list(model) && !is.null(model$lhs)) {
    if (is.null(model$mod.idx)) {
      partable <- model
    }
  } else {
    args <- list(
      model = model,
      warn = TRUE,
      debug = FALSE,
      auto = (call != "lavaan"),
      ...
      )
    partable <- do.call(lavaanify, args)
  }

  # STEP 1 - Classify model
  model_type <- classify_model(partable)

  if (is.na(model_type) | model_type == "mlm") {
    stop("This model type is not currently supported")
  }

  # STEP 2 - Evaluate general rules
  rules <- lapply(
    get_rules(model_type = "sem"),
    function(rule) do.call(rule, list(partable))
    )
  nsem <- length(rules)
  # STEP 3 - Evaluate model type-specific rules (if applicable)
  special <- isTRUE(model_type %in% c("cfa", "reg"))
  if (special) {
    rules <- c(
      rules,
      lapply(
        get_rules(model_type = model_type),
        function(rule) do.call(rule, list(partable))
      )
    )
  }
  # STEP 4 - Print Output
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
      format(
        paste0(ifelse(i <= nsem, "", "."),
          rules[[i]]$rule),
        width = wind - 34L
        ),
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
    cat("---\nWarnings:\n")
    for (i in 1:widx) {
      w0 <- strwrap(wrns[i], width = wind - 4L)
      ls <- length(w0)
      w0[1] <- paste0(sprintf("%s - ", i), w0[1])
      if (ls > 1L) {
        w0[2:ls] <- paste0(rep(strrep(" ", 4L), ls - 1L), w0[2:ls])
      }
      cat(w0, sep = "\n")
    }
  }
  return(invisible(rules))
}
