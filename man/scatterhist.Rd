\name{scatterhist}
\alias{scatterhist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scatter plot with marginal histograms
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produces a scatter plot for two variables, with histograms in the margins of the plot.
}
\usage{
scatterhist(x, y, xlab = "", ylab = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Vector of values to be put on the x-axis.
}
  \item{y}{
Vector of values to be put on the y-axis.
}
  \item{xlab}{
String, label of x-axis.
}
  \item{ylab}{
String, label of y-axis.
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
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Theo Pepler
}
\note{
Source: Adapted from the function given at http://sas-and-r.blogspot.com/2011/06/example-841-scatterplot-with-marginal.html.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(iris)
scatterhist(x = iris$Petal.Length,
  y = iris$Petal.Width,
  xlab = 'Petal length (cm)',
  ylab = 'Petal width (cm)')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
