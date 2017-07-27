auth.connect <- function(DSN, creds.file = NULL, UID = NULL)
{
  if (!requireNamespace("RODBC", quietly = TRUE)) {
    stop("The package 'RODBC' is needed for this function to work. Install before proceeding.")
  }

  library(RODBC)

  # Either get ID and password from file, or prompt for it
  if (is.null(creds.file)) {

    if (is.null(UID)) {
      UID <- readline("Enter DSN username: ")
    }
    PWD <- readline("Enter DSN password: ")

  } else {
    creds <- read.csv(creds.file, header = TRUE)
    UID <- as.character(subset(creds, DataSourceName = DSN)[2])
    PWD <- as.character(subset(creds, DataSourceName = DSN)[3])
    rm(creds)

  }

  channel <- odbcConnect(dsn=DSN, uid=UID, pwd=PWD)

  rm(UID)
  rm(PWD)

  return(channel)
}








