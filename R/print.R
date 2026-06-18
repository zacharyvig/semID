#' Printing function for rules list
#'
#' @param x A \code{lavid} object, i.e., a list of rule output objects.
#' @param names Character vector. The names of the columns of the main table
#' @param include.msgs Logical. If \code{TRUE} messages are printed
#' @param msgs.name Character. The name of the messages index column
#' @param msgs.sec Character. The name of the messages section
#' @param msgs.levels Named character vector. The names of the message levels,
#'        e.g., "1" = "Info", "2" = "Reason", "3" = "WARNING".
#' @param window Integer. The width of the output window
#' @param pos.lab Character. The label for positive cells, e.g., "Yes".
#' @param neg.lab Character. The label for negative cells, e.g., "No".
#' @param na.lab Character. The label for NA/blank cells.
#' @param ... Not currently used.
#'
#' @export
print.semid <- function(x, ..., names = c("", "Pass", "Necessary", "Sufficient"),
                        include.msgs = TRUE, msgs.name = "Message", msgs.sec = "Messages",
                        msgs.levels = c("1" = "Info", "2" = "Reason", "3" = "WARNING"),
                        window = 56L, pos.lab = "Yes", neg.lab = "No", na.lab = "-") {
  if (!is.null(x$print.options$include.msgs)) {
    if (x$print.options$include.msgs != include.msgs) {
      warning("The `include.msgs` argument in the print method is overriding the `include.msgs` argument in the semid object.")
    } else {
      include.msgs <- x$print.options$include.msgs
    }
  }
  if (include.msgs) {
    names[5] <- msgs.name
    msgs <- character(0) # global message vector
  }
  cols_width <- sum(nchar(names[-1])) + length(names[-1])
  names[1] <- format(names[1], width = window - cols_width)
  widx <- 0L # global warning index

  version <- utils::packageVersion("semID")
  cat(sprintf("semID %s Rule Check\n\n", version))

  cat(names, strrep("\n", 1L))

  for (i in seq_along(x$Rules)) {
    this_rule <- x$Rules[[i]]
    row <- c(
      # rule title
      format(
        this_rule$rule,
        width = window - cols_width
      ),
      # did the rule pass?
      format(
        switch(
          as.character(this_rule$pass),
          "NA" = na.lab,
          "TRUE" = pos.lab,
          "FALSE" = neg.lab),
        width = nchar(names[2]), justify = "right"
      ),
      # necessary and/or sufficient?
      switch(
        as.character(this_rule$cond),
        "N" = c(pos.lab, neg.lab),
        "S" = c(neg.lab, pos.lab),
        "NS" = c(pos.lab, pos.lab),
        "NA" = rep(na.lab, 2)
      )
    )
    row[3:4] <- c(
      format(row[3], width = nchar(names[3]), justify = "right"),
      format(row[4], width = nchar(names[4]), justify = "right")
    )
    if (include.msgs && all(!is.na(this_rule$msgs))) {
      idx.w0 <- c()
      for (msg in this_rule$msgs) {
        if (msg %in% msgs) {
          # prevent duplicate messages
          idx.w0 <- c(idx.w0, which(msgs == msg))
        } else {
          widx <- widx + 1
          idx.w0 <- c(idx.w0, widx)
          msgs <- c(msgs, msg)
        }
      }
      cat(
        c(row, format(
          paste(idx.w0, collapse = ","),
          width = nchar(names[5]), justify = "right")),
        "\n"
      )
    } else {
      cat(row, "\n")
    }
  }

  if (include.msgs && widx > 0) {
    cat("---", msgs.sec, sep = "\n")
    for (i in 1:widx) {
      # make space for index, e.g., "1 - ", "2 - ", etc.
      m0 <- strwrap(msgs[i], width = window - 4)
      ls <- length(m0)
      m0[1] <- paste0(sprintf("%s - ", i), m0[1])
      if (ls > 1L) {
        m0[2:ls] <- paste0(
          rep(strrep(" ", 4L), ls - 1),
          m0[2:ls]
        )
      }
      cat(m0, sep = "\n")
    }
  }

  cat("\n")

  return(invisible(x))

}


#' Printing function for scaling table
#' 
#' @param x A \code{semscale} object, i.e., a list of scaling information
#' for each latent variable in the model.
#' @param include.msgs Logical. If \code{TRUE} messages are printed.
#' @param window Integer. The width of the output window.
#' @param indent.lens Integer vector of length 3. The number of spaces to indent
#' for each level of information (currently there are three supported).
#' @param na.lab Character. The label for NA/blank cells.
#' @param pos.lab Character. The label for positive cells, e.g., "Yes".
#' @param neg.lab Character. The label for negative cells, e.g., "No".
#' @param empty.lab Character. The label for empty cells, e.g., "None".
#' @param bullet Character. The bullet symbol for messages, e.g., "-".
#' @param ... Not currently used.
#'
#' @export
print.semscale <- function(x, ..., include.msgs = TRUE, window = 56L,
                           indent.lens = c(0L, 2L, 2L), na.lab = "na",
                           pos.lab = "Yes", neg.lab = "No", empty.lab = "None",
                           bullet = "-") {
                            
  stopifnot("`indent.lens` must be three equal or ascending integers" =
    length(indent.lens) == 3 && indent.lens[2] >= indent.lens[1] && indent.lens[3] >= indent.lens[2])

  if (length(x) ==1 && is.na(x)) {
    cat("No latent variables in the model\n")
    return(invisible(x))
  }
  if (!is.null(x$print.options$include.msgs)) {
    if (x$print.options$include.msgs != include.msgs) {
      warning("The `include.msgs` argument in the print method is overriding the `include.msgs` argument in the semid object.")
    } else {
      include.msgs <- x$print.options$include.msgs
    }
  }

  scaling <- x$Scaling
  indents <- strrep(" ", indent.lens)

  version <- utils::packageVersion("semID")
  cat(sprintf("semID %s Latent Variable Scaling\n\n", version))

  for (i in seq_along(scaling)) {
    var <- scaling[[i]]$lv
    cat(sprintf("%s%s\n", indents[1], var))

    is.scaled <- switch(
      as.character(scaling[[i]]$scaled),
      "NA" = na.lab,
      "TRUE" = pos.lab,
      "FALSE" = neg.lab
    )
    
    n.ind <- as.integer(scaling[[i]]$n.indicators)
    scale.ind <- ifelse(
      is.na(scaling[[i]]$scaling.indicator), empty.lab, scaling[[i]]$scaling.indicator
    )

    cat(sprintf("%sLV is scaled: %s\n", indents[2], is.scaled))
    cat(sprintf("%sNo. of indicators: %s\n", indents[2], n.ind))
    cat(sprintf("%sScaling indicator: %s\n\n", indents[2], scale.ind))

    if (include.msgs) {
      if (isTRUE(scaling[[i]]$scaled)) {
        cat(sprintf("%sScaling method(s):\n", indents[2]))
        cat(paste0(indents[3], bullet, " ", scaling[[i]]$scaling.method, collapse = "\n"))
      } else {
        cat(sprintf("%sScaling error:\n", indents[2]))
        cat(paste0(indents[3], bullet, " ", scaling[[i]]$fail.reason))
      }
      cat("\n\n")
    }

  }

  return(invisible(x))

}