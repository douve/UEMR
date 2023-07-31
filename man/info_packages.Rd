\name{info_packages}
\alias{info_packages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Package availability
}
\description{
Shows whether or not a package is available for the current version of R.
}
\usage{
info_packages(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Named vector of the packages to be informed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of two elements. The first one is a LOGICAL vector indicating wheter or not the package is available. Second element is a message specifying if the packages are available or not, and in case of not being them, it indicates which ones.
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
info_packges(c("dplyr","data.table","ggplot2"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
