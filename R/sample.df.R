sample.df <- function(df, size, strata=NULL) {

  if (is.null(strata)) {
    # Simple random sample

    if (size < 1) {size <- round(nrow(df) * size, 0)}
    sample <- df[sample(nrow(df), size),]

  } else {
    # Stratified random sample

    # store strata in list
    v.interaction <- interaction(df[strata], drop=TRUE)
    ls.split <- split(df, v.interaction)

    # calculate sizes, and store in list
    if (size < 1) {
      ls.sizes <- as.list(round(table(v.interaction) * size, 0))

    } else {

      # create list of the same length as number of strata, each element being
      # filled with the user-defined size
      ls.sizes <- list()
      for (i in 1:length(ls.split)) {
        if (size > nrow(ls.split[[i]])) {
          ls.sizes[i] <- 0
          warning("Strata cell contains fewer records than specified sample size. No observations selected.")
        } else {
          ls.sizes[i] <- size
        }
      }
    }

    # Select correctly sized samples from each strata cell
    msample <- function(df, size) list(df[sample(nrow(df), size),])
    samples <- mapply(msample, ls.split, ls.sizes)

    # Combine strata samples
    sample <- Reduce(rbind, samples)
  }

  return(sample)
}
