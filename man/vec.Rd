\name{vec}
\alias{vec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Vector of matrix elements
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Stacks the elements of a matrix row-wise in a vector.
}
\usage{
vec(datamat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datamat}{
%%     ~~Describe \code{datamat} here~~
A matrix.
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
Returns a vector containing the elements in the stacked rows of \code{datamat}.
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
function (datamat) 
{
    n <- nrow(datamat)
    outputvec <- NULL
    for (i in 1:n) {
        outputvec <- append(outputvec, datamat[i, ])
    }
    return(outputvec)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
