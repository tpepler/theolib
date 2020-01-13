\name{standcol}
\alias{standcol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standardise the columns of a matrix
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Enables the centering and scaling of the columns of a matrix, to have zero mean and unit variance.
}
\usage{
standcol(x, centre = FALSE, stand = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
Matrix to be centred and/or scaled.
}
  \item{centre}{
%%     ~~Describe \code{centre} here~~
Should the columns of \code{x} be centred to have zero mean? (TRUE/FALSE)
}
  \item{stand}{
%%     ~~Describe \code{stand} here~~
Should the columns of \code{x} be scaled to have unit variance? (TRUE/FALSE)
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
Returns matrix \code{x} with the columns centred and/or scaled.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Theo Pepler
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, centre = FALSE, stand = TRUE) 
{
    if (centre) {
        for (i in 1:ncol(x)) {
            x[, i] <- x[, i] - mean(x[, i])
        }
    }
    if (stand) {
        for (i in 1:ncol(x)) {
            x[, i] <- x[, i]/sd(x[, i])
        }
    }
    return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
