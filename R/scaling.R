#' Check if latent variables are correctly scaled
#'
#' This function checks whether latent variables in the model are scaled,
#' which is a necessary condition for model identification. A latent variable
#' with one indicator is not scaled, though there are methods to achieve identification
#' using a known reliability coefficient (see Bollen, 2026). This function always
#' returns \code{FALSE} in the case of one indicator. A latent variable with two indicators
#' is scaled if both loadings are fixed. A latent variable with three or more indicators is
#' scaled if (a) it has a scaling indicator (an indicator with a fixed loading) which
#' also has a fixed mean, (b) it has a scaling indicator and its variance is fixed,
#' or (c) both its mean and variance are fixed. If any latent variable is not scaled,
#' the model is not identified.
#'
#' @param x A \code{lavaan} parameter table or a \code{semID} ID object.
#' @param call A character string specifying the call you intend to use to fit
#'  the model. This will ensure the correct model defaults are specified. Options
#'  currently include "lavaan", "sem", or "cfa". If a parameter table for fit
#'  object are supplied, this argument is ignored.
#' @param include.msgs Logical. If \code{TRUE} the output will print why a 
#' latent variable is not scaled when applicable and the method or methods
#' by which it is scaled when it is scaled. If \code{FALSE}, the output will
#' only print whether the latent variable is scaled or not and a few other
#' descriptives.
#' @param lv An optional character vector of the specific latent variables you
#'  would like to check. Otherwise, all latent variables are extracted from the
#'  parameter table
#' @param return.type A character string specifying the type of output. Options
#'  include "logical" (a logical vector specifying whether each latent variable is
#'  correctly scaled) or "table" (a data frame with additional information about the
#'  scaling of each latent variable). The latter is of type \code{scaling.table} and
#'  has a custom print method for additional diagnosis of identification issues.
#' @param ... Additional arguments passed to the \code{lavaanify} function from
#'  \code{lavaan}. See \link[lavaan]{lavaanify} for more information. These are
#'  only used when a model string is supplied. Otherwise, they are ignored.
#'
#' @return An object of class \code{scaling.table} (if \code{return.type = "table"})
#' or a logical vector (if \code{return.type = "logical"}) indicating whether each
#' latent variable is correctly scaled.
#'
#' @export
#' @name scaling
#' @examples
#' \dontrun{
#'  mod1 <- ' l1 =~ x1 + x2
#'            l2 =~ x3 + x4 + x5 '
#'  mod1.partable <- lavaan::lavaanify(mod1, auto = TRUE, model.type = "sem")
#'  mod1.scaled <- scaling(mod1.partable, lv = "l2")
#' }
#'
scaling <- function(x, call = "sem", include.msgs = TRUE, lv = NULL, 
                    return.type = c("object", "logical"), ...) {                  
  stopifnot(
    "Argument `lv` must be a character vector or NULL" =
      is.character(lv) || is.null(lv)
  )
  stopifnot(
    "Argument `include.msgs` must be a logical" =
      is.logical(include.msgs)
  )
  stopifnot(
    "Unknown `call` or `call` currently not supported" =
      call %in% c("lavaan", "sem", "cfa")
  )
  UseMethod("scaling")
}

#' @rdname scaling
#' @export
scaling.semid <- function(x, call = "sem", include.msgs = TRUE, lv = NULL, 
                          return.type = c("object", "logical"), ...) {
  print.semid(x)
  return(scaling.data.frame(x$partable, include.msgs = include.msgs, lv = lv, return.type = return.type))
}

#' @rdname scaling
#' @export
scaling.lavaan <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                           return.type = c("object", "logical"), ...) {
  dotdotdot <- list(...)
  if (length(dotdotdot) > 0) {
    warning("Additional arguments are ignored when a fitted lavaan object is supplied")
  }
  partable <- as.data.frame(
    x@ParTable,
    stringsAsFactors = FALSE
  )
  return(scaling.data.frame(partable, include.msgs = include.msgs, lv = lv, return.type = return.type))
}

#' @rdname scaling
#' @export
scaling.character <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                              return.type = c("object", "logical"), ...) {
  dotdotdot <- list(...)
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
  args <- c(
    list(
      model = x,
      warn = TRUE,
      debug = FALSE,
      model.type = "sem"
    ),
    dotdotdot
  )
  partable <- do.call(lavaan::lavaanify, args)
  return(scaling.data.frame(partable, include.msgs = include.msgs, lv = lv, return.type = return.type))
}

#' @rdname scaling
#' @export
scaling.data.frame <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                               return.type = c("object", "logical"), ...) {
  return.type <- match.arg(return.type) 
  if (!(is.list(x) && !is.null(x$lhs) && is.null(x$mod.idx))) {
    stop("Unknown list format. Please supply a lavaan parameter table or fitted model object.")
  }
  
  if (is.null(lv)) {
    # retrieve attributes and variable names
    vars <- get_partable_vars(x, "lv")
    lv <- vars$lv
  }
  if (length(lv) == 0) {
    stop("Scaling only applies to models with latent variables")
  }

  if (return.type == "object") {
    scaling.table <- list(
      lv = NA_character_,
      scaled = NA,
      n.indicators = NA_integer_,
      scaling.indicator = NA_character_,
      fail.reason = NA_character_,
      scaling.method = NA_character_
    )
    scaling.tables <- replicate(length(lv), scaling.table, simplify = FALSE)
  } else {
    out <- vector(length = length(lv))
    names(out) <- lv
  }

  meanstructure <- any(x$op == "~1")

  for (i in seq_along(lv)) {
    var <- lv[i]
    if (return.type == "object") {
      scaling.tables[[i]]$lv <- var
    }
    # one-indicator case
    one.ind <- sum(with(x, lhs == var & op == "=~" & rhs != var)) == 1
    if (one.ind) {
      if (return.type == "object") {
        scaling.tables[[i]]$scaled <- FALSE
        scaling.tables[[i]]$n.indicators <- 1
        scaling.tables[[i]]$fail.reason <- "One indicator"
      } else {
        out[var] <- FALSE
      }
      next
    }
    # two-indicator case / no lv correlations
    two.ind <- sum(with(x, lhs == var & op == "=~")) == 2
    cov.lv <- any(with(x, (lhs == var & rhs != var) |
                         (rhs == var & lhs != var) & op == "~~"))
    if (two.ind && !cov.lv) {
      check0 <- sum(
        with(x, lhs == var & op == "=~" & free == 0)
        ) == 2
      if (return.type == "object") {
        scaling.tables[[i]]$scaled <- check0
        scaling.tables[[i]]$n.indicators <- 2
        if (!check0) {
          scaling.tables[[i]]$fail.reason <- "Two indicators but one or both loadings not fixed"
        } else {
          scaling.tables[[i]]$scaling.method <- "Two indicators with fixed loadings"
        }
      } else {
        out[var] <- check0
      }
      next
    }
    # scaling indicator?
    scale.ind.idx <- with(x, lhs == var & op == "=~" &
                            free == 0 & rhs != var)
    check1 <- check2 <- any(scale.ind.idx)                            
    if (check1) {
      scale.ind <- with(x, rhs[scale.ind.idx])
      # scaling indicator mean fixed?
      check2 <- ifelse(
        meanstructure,
        any(with(x, lhs %in% scale.ind & op == "~1" & free == 0)),
        TRUE
      )
    }
    # latent variance fixed?
    check3 <- any(
      with(x, lhs == var & rhs == var & op == "~~" & free == 0)
    )
    # latent mean fixed?
    check4 <- ifelse(
      meanstructure,
      with(x, lhs == var & op == "~1" & free == 0),
      TRUE
    )
    # final check: either (1 and 2) or (3 and 4) or (1 and 3)
    final_check <- c(check1 && check2, check3 && check4, check1 && check3)
    if (return.type == "logical") {
      out[var] <- any(final_check)
      next
    }
    # if table requested...
    scaling.tables[[i]]$scaled <- TRUE
    scaling.tables[[i]]$n.indicators <- sum(with(x, lhs == var & op == "=~" & rhs != var))
    if (check1) {
      scaling.tables[[i]]$scaling.indicator <- scale.ind
    }
    scaling.methods <- c("Scaling indicator with fixed mean",
                         "Latent variable with fixed mean and variance",
                         "Scaling indicator with fixed variance")
    scaling.tables[[i]]$scaling.method <- scaling.methods[which(final_check)]

    if (!any(final_check)) {
      scaling.tables[[i]]$scaled <- FALSE
      if (!check1) {
        if (check3 && !check4) {
          scaling.tables[[i]]$fail.reason <- "Latent variance fixed but no scaling indicator/fixed latent mean"
        } else if (!check3 && check4) {
          scaling.tables[[i]]$fail.reason <- "Latent mean fixed but latent variance not fixed"
        } else {
          scaling.tables[[i]]$fail.reason <- "Neither scaling indicator nor fixed latent variance/mean"
        }
      } else {
        scaling.tables[[i]]$fail.reason <- "Scaling indicator mean and/or latent variable mean must be fixed"
      }
    }
  }

  if (return.type == "logical") {
    return(out)
  } else {
    out <- list(
      Scaling = scaling.tables,
      partable = x,
      print.options = list(
        include.msgs = include.msgs
      )
    )
    class(out) <- c("semscale")
    return(out)
  }
}


#' @rdname scaling
#' @export
scaling.default <- function(x, call = "sem", include.msgs = TRUE, lv = NULL,
                            return.type = c("table", "logical"), ...) {
  stop("Unknown object type. Please supply a model string, lavaan parameter table, or fitted model object.")
}