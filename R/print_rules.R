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
#' @param ... Not currently used
#'
#' @export
print.lavid <- function(x, ..., names = c("", "Pass", "Necessary", "Sufficient"),
                        include.msgs = TRUE, msgs.name = "Message", msgs.sec = "Messages",
                        msgs.levels = c("1" = "Info", "2" = "Reason", "3" = "WARNING"),
                        window = 56L, pos.lab = "Yes", neg.lab = "No", na.lab = "-") {
  if (!is.null(attr(x, "include.msgs"))) {
    include.msgs <- attr(x, "include.msgs")
  }
  if (include.msgs) {
    names[5] <- msgs.name
    msgs <- c() # global message vector
  }
  cols_width <- sum(nchar(names[-1])) + length(names[-1])
  names[1] <- format(names[1], width = window - cols_width)
  widx <- 0 # global warning index

  cat(names, strrep("\n", 1L))

  for (i in 1:length(x)) {
    this_rule <- x[[i]]
    row <- c(
      # rule title
      format(
        this_rule$rule,
        width = window - cols_width
      ),
      # pass?
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
      m0 <- strwrap(msgs[i], width = window - 4L)
      ls <- length(m0)
      m0[1] <- paste0(sprintf("%s - ", i), m0[1])
      if (ls > 1L) {
        m0[2:ls] <- paste0(
          rep(strrep(" ", 4L), ls - 1L),
          m0[2:ls]
        )
      }
      cat(m0, sep = "\n")
    }
  }

  cat("\n")

}
