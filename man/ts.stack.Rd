\name{ts.stack}
\alias{ts.stack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Stack time series
%%  ~~function to do ... ~~
}
\description{Utility function to stack a vector of time series data in a matrix, to be used in spectral analysis of the time series, for example.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ts.stack(x, window = floor(length(x)/2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector containing the time series data.
%%     ~~Describe \code{x} here~~
}
  \item{window}{Window length of interest. The number of rows of the resultant matrix will be equal to this value.
%%     ~~Describe \code{window} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a matrix with the number of rows equal to \code{window}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Theo Pepler
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
xvec <- c(1:20)
ts.stack(x = xvec, window = 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
