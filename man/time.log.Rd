\name{time.log}
\alias{time.log}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Time Log
}
\description{
Creates a background table for logging and reading times. This can be useful for calculating the
  run time of code, or particularly, for estimating the time remaining in an iterative process.
}
\usage{
time.log(action = "log", unit = "mins", comment = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{action}{
Character string stating the action to be performed. Accepts 3 values: "log", "return", and "clear"
  "log" appends the table with a new row containing the current system time, as well as calculations
  for iteration time and total run time. "return" returns the log for viewing or storing as a
  dataframe. "clear" Clears the log.
}
  \item{unit}{
Base unit to use when measuring time. Accepts "secs",mins","hours","days", and "weeks".
}
  \item{comment}{
Character string containing any comment the user may want to in that line of the time log.
}
}
\details{
}
\value{
Data frame containing times logged, stored in the background of the current instance.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
