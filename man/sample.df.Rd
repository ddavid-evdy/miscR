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
sample.df(df, size, strata=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Data frame for selecting stratified random sample.
}
  \item{size}{
Size of sample to select. If less than 1, a percent will be selected. If
  greater than 1 an absolute number will be selected.
}
  \item{strata}{
String, or vector of strings, containing names of fields in df on which to
  stratify. If NULL, selects a simple random sample.
}
}
\details{
If strata are specified, and an absolute size is given which is larger than
  the total number of observations in a given strata cell, no records will be
  selected from that cell.
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
#
dataset <- mtcars
sample.df(dataset, 0.10)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
