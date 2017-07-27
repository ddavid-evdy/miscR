\name{auth.connect}
\alias{auth.connect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Connecting to a Data Source that Requires Authentication
}
\description{
Creates connection to an external database that requires authentication,
  without having to write credentials into the code itself.
}
\usage{
auth.connect(DSN, creds.file = NULL, UID = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DSN}{
Character string. A registered data source name.
}
  \item{creds.file}{
Character string. The full file path of the file containing DSNs and
  credentials. If null, the user will be prompted for a username and password
  in the console.
}
  \item{UID}{
Character string. The username for the data source connection.
}
}
\details{
This function is a wrapper to the RODBC package's odbcConnect function.
  Although that function is convenient for connecting to external data sources,
  one disadvantage is that it requires the user to state username and password
  in the function call for any DNS which requires authentication.

Understandably, the user may not want to store those credentials in the code,
  or may find it cumbersome to remember to delete them after the code is run.
  There are ways that you can store that information in the DNS file itself,
  but you may not be comfortable doing so, or may not know how.

auth.connect allows you to connect to an external database that requires
  authentication, without having to type credentials into the code itself. It
  can be done in one of two ways, the user can be prompted in the console
  window (which is automatically cleared once the session is closed), or they
  can provide the file path to a credentials file stored on their local
  machine (this, of course, is the least secure option).

If using the prompts, the username can optionally be added into the function
  call, so the console only prompts for a password. If null, the function will
  prompt for both.

If using a credentials file, it must be saved as a csv, and structured in a
  specific way. Run 'View(CredsExample)' in the console window to view an
  example of how the file should be structured.
}
\value{
A non-negative integer which is used as a handle if no error occurred, -1
  otherwise. A successful return has class "RODBC", and attributes including:

  \item{connection.string}{
the full ODBC connection string.
}
  \item{case}{
the value of case.
}
  \item{id}{
a numeric ID for the channel.
}
  \item{believeNRows}{
the value of believeNRows.
}
  \item{rows_at_time}{
the value of rows_at_time.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# To connect using a locally stored credentials file
auth.connect(DSN = "SomeDSN", creds.file = "C:/Users/John Smith/Documents/CredsFile.csv")

# To be prompted for appropriate username and password
auth.connect(DSN = "SomeDSN")

# To be prompted for appropriate password only
auth.connect(DSN = "DSN2", UID = "jsmith")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
