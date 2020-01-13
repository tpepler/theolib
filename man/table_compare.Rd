\name{table_compare}
\alias{table_compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Construct a 2x2 contingency table and calculate associated statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Construct a contingency table to compare predicted outcomes to actual/observed outcomes. Also calculates statistics associated with the table.
}
\usage{
table_compare(pred, actual)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pred}{
%%     ~~Describe \code{pred} here~~
Predicted outcomes (0/1), as obtained from a model or decision rule.
}
  \item{actual}{
%%     ~~Describe \code{actual} here~~
Actual observed outcomes (0/1).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a list with the following:
\item{cont.table }{The 2x2 contingency table}
\item{sens }{Sensitivity}
\item{spec }{Specificity}
\item{misclass.rate }{Misclassification rate}
\item{false.pos }{False positive rate}
\item{false.neg }{False negative rate}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Theo Pepler
}
\note{
%%  ~~further notes~~
Note that the calculations assume that a "positive" result is coded as a 1, and a "negative" result as a 0.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
predict.outcomes <- c(1, 1, 0, 1, 1, 1, 0, 0)
actual.outcomes <- c(1, 1, 1, 1, 0, 0, 0, 0)
table_compare(pred = predict.outcomes, actual = actual.outcomes)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
