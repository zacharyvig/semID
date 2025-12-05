#' Printing function for rules list
#'
#' @param rules A rules list produced by internal identification rules
#' @param names Character vector. The names of the columns of the main table
#' @param warn Logical. If \code{TRUE} warnings are printed
#' @param warn.col Character. The name of the warning index column
#' @param warn.sec Character. The name of the warnings section
#' @param window Integer. The width of the output window
#' @param pos.lab Character. The label for positive cells, e.g., "Yes".
#' @param neg.lab Character. The label for negative cells, e.g., "No".
#' @param na.lab Character. The label for NA/blank cells.
#'
#' @keywords internal
#' @author Zach Vig
print_rules <- function(rules, names = c("", "Pass", "Necessary", "Sufficient"),
                        warn = TRUE, warn.name = "Warning", warn.sec = "Warnings",
                        window = 56L, pos.lab = "Yes", neg.lab = "No", na.lab = "-") {
  names <- c(
    "", "Pass", "Necessary", "Sufficient"
  )
  if (warn) {
    names[5] <- warn.name
    wrns <- c()
  }
  cols_width <- sum(nchar(names[-1])) + length(names[-1])
  names[1] <- format(names[1], width = window - cols_width)
  widx <- 0 # warning index

  cat(names, strrep("\n", 1L))

  for (i in 1:length(rules)) {
    row <- c(
      # rule title
      format(
        rules[[i]]$rule,
        width = window - cols_width
      ),
      # pass?
      format(
        switch(
          as.character(rules[[i]]$pass),
          "NA" = na.lab,
          "TRUE" = pos.lab,
          "FALSE" = neg.lab),
        width = nchar(names[2]), justify = "right"
      ),
      # necessary and/or sufficient?
      switch(
        as.character(rules[[i]]$cond),
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
          width = nchar(names[5]), justify = "right")),
        "\n"
      )
    } else {
      cat(row, "\n")
    }
  }

  if (warn & widx > 0) {
    cat("---", warn.sec, sep = "\n")
    for (i in 1:widx) {
      w0 <- strwrap(wrns[i], width = window - 4L)
      ls <- length(w0)
      w0[1] <- paste0(sprintf("%s - ", i), w0[1])
      if (ls > 1L) {
        w0[2:ls] <- paste0(
          rep(strrep(" ", 4L), ls - 1L),
          w0[2:ls]
        )
      }
      cat(w0, sep = "\n")
    }
  }

  cat("\n")

}
