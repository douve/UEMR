\name{prepare.packages}
\alias{prepare.packages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare packages to load
}
\description{
Install or update packages.
}
\usage{
prepare.packages(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Named vector of the packages to be loaded.
}
  \item{update}{
Default is FALSE. if TRUE then the packages that have an update available will also be updated.
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
prepare.packages(c("dplyr","data.table","tidyverse"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
