\name{sample.df}
\alias{sample.df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select Random Sampling from Data Frame
}
\description{
Returns a random sample of a given data frame, either simple or stratified,
  depending on inputs.
}
\usage{
sample.df(df, size, strata = NULL, sample.only = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame for selecting stratified random sample.
}
  \item{size}{
Size of sample to select. If less than 1, a percent will be selected. If greater than 1, an
  absolute number will be selected.
}
  \item{strata}{
String, or vector of strings, containing names of fields in df on which to stratify. If NULL,
  selects a simple random sample.
}
  \item{sample.only}{
Flag for returning entire dataframe with sample marked, or sample only. If NULL, returns entire
  dataframe with sample marked.
}
}
\details{
If strata are specified, and an absolute size is given which is larger than the total number of
  observations in a given strata cell, no records will be selected from that cell.
}
\value{
Data frame containing selected observations.
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
# Return whole dataframe with 10\% of observations randomly marked
sample.df(workhrs, 0.10)

# Return randomly selected 12\% of the dataframe
sample.df(workhrs, 0.10, sample.only = TRUE)

# Return 21 randomly selected observations
sample.df(workhrs, 21, sample.only = TRUE)

# Return randomly selected 30\% of the dataframe, distributed proportionally across marital.status
sample.df(workhrs, 0.30, strata = "marital.status", sample.only = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
