\name{box.mtest}
\alias{box.mtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Box's M test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Chi-square approximation to Box's M test for equality of k covariance matrices, as described in Rencher (2002, p255).
}
\usage{
box.mtest(covmats, nvec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{covmats}{
%%     ~~Describe \code{covmats} here~~
Array of k covariance matrices to be tested for equality.
}
  \item{nvec}{
%%     ~~Describe \code{nvec} here~~
Vector of sample sizes of the k groups.
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
Returns a list with the values
\item{p.value }{The p value for the test.}
\item{S.pooled }{Pooled covariance matrix as used in the M-test, under the assumption of equal population covariance matrices.}
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
## Iris data
data(iris)
iris1 <- subset(iris, Species == 'setosa')
iris2 <- subset(iris, Species == 'versicolor')
iris3 <- subset(iris, Species == 'virginica')
## Array containing the covariance matrices
S <- array(NA, dim = c(4, 4, 3))
S[, , 1] <- cov(iris1[, 1:4])
S[, , 2] <- cov(iris2[, 1:4])
S[, , 3] <- cov(iris3[, 1:4])
## Box's M-test
box.mtest(covmats = S, nvec = c(50, 50, 50))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
