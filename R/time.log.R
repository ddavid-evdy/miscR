time.log <- local({

  df.time <- data.frame(
                          time       = as.POSIXct(character()),
                          row.time   = numeric(),
                          total.time = numeric(),
                          comment    = character(),
                          stringsAsFactors = FALSE
                       )
  u <- "mins"

  function(action = "log", unit = NULL, comment = NULL) {

    if (action == "log") {
      df.time[nrow(df.time) + 1, "time"] <<- Sys.time()
      row <- nrow(df.time)

      if (row == 1) {
        u <<- unit
        df.time[row, 2] <<- 0
        df.time[row, 3] <<- 0
      } else {

        if (u == unit || is.null(unit)) {
          # Calculating row.time, then total.time
          df.time[row, 2] <<- round(difftime(df.time[row, 1], df.time[row - 1, 1], units=u), 3)
          df.time[row, 3] <<- round(df.time[row - 1, 3] + df.time[row, 2], 3)
        } else {
          stop("Cannot alter unit after log is created")
        }
      }
      df.time[nrow(df.time), "comment"] <<- comment

    } else if (action == "return") {
      return(df.time)

    } else if (action == "clear") {
      df.time <<- df.time[0, ]

    } else {
      stop("Action not recognized. See help file for list of acceptable inputs.")
    }
  }
})
