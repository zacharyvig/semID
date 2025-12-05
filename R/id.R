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
#'  currently include "lavaan", "sem", or "cfa". If a parameter table for fit
#'  object are supplied, this argument is ignored.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'  \code{lavaan}. See \link[lavaan]{lavaanify} for more information.
#'
#' @return The \code{lavaan} parameter table for the model (invisibly).
#'
#' @importFrom lavaan lavaanify
#'
#' @examples
#' # Holzinger and Swineford (1939) example
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' id(HS.model, warn = TRUE, call = "cfa", meanstructure = FALSE)
#' @export
id <- function(model = NULL, warn = TRUE, call = "sem", ...) {
  dotdotdot <- list(...)
  # Checks
  stopifnot(
    "Argument `warn` must be a logical" =
      is.logical(warn)
  )
  stopifnot(
    "Unknown `call` or `call` currently not supported" =
      call %in% c("lavaan", "sem", "cfa")
  )
  if (isTRUE(dotdotdot$model.type == "efa")) {
    dotdotdot[["model.type"]] <- NULL
    warning("Only `model.type='sem'` is currently supported")
  }
  if (isTRUE(dotdotdot$debug)) {
    dotdotdot[["debug"]] <- NULL
    warning("Ignoring `debug`")
  }
  if (is.null(dotdotdot$auto)) {
    dotdotdot$auto <- (call != "lavaan")
  }

  # STEP 0 - Parse input
  if (inherits(model, "lavaan")) {
    partable <- as.data.frame(
      model@ParTable,
      stringsAsFactors = FALSE
      )
    input <- "fit"
  # check if already partable (from lav_partable)
  } else if (is.list(model) && !is.null(model$lhs)) {
    if (is.null(model$mod.idx)) {
      partable <- model
      input <- "partable"
    }
  } else {
    args <- c(
      list(
        model = model,
        warn = TRUE,
        debug = FALSE,
        model.type = "sem"
      ),
      dotdotdot
    )
    partable <- do.call(lavaanify, args)
    input <- "syntax"
  }

  # STEP 1 - Classify model
  model_type <- classify_model(partable)
  if (is.na(model_type) | model_type == "mlm") {
    stop("This model type is not currently supported")
  }
  if(input == "syntax") {
    if (call == "cfa" & call != model_type) {
      warning("`sem()` or `lavaan()` may be more appropirate calls for this type of model")
    }
  }

  # STEP 2 - Evaluate rules
  rules <- c(
    lapply(
      get_rules(model_type = "sem"),
      function(rule) do.call(rule, list(partable))
    ),
    lapply(
      get_rules(model_type = "cfa"),
      function(rule) do.call(rule, list(partable))
    ),
    lapply(
      get_rules(model_type = "reg"),
      function(rule) do.call(rule, list(partable))
    )
  )

  # STEP 3 - Print Output
  print_rules(
    rules = rules,
    names = c("", "Pass", "Necessary", "Sufficient"),
    warn = warn,
    warn.name = "Warning",
    warn.sec = "Warnings",
    window = 56L,
    pos.lab = "Yes",
    neg.lab = "No",
    na.lab = "-"
  )

  return(invisible(partable))
}
