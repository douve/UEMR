\name{dots_f}
\alias{dots_f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Returns arguments that do belong to a specific function or set of functions
}
\usage{
dots_f(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{
List of packages with their respectives functions
}
\item{args}{
list of entry arguments
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
# dots is a set of possible arguments
dots <- list(na.strings = "", encoding = "UTF-8", keepFormulas = FALSE)
dots_f(list(data.table="fread"), dots) # we get the arguments of dots that do belong to fread function.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
