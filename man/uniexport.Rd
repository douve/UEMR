\name{uniexport}
\alias{uniexport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
uniexport
}
\description{
Exports files in 5 different data types (Robject, Rimage, excel, csv and textfile)
}
\usage{
uniexport(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Robject}{
the object to be writen. It must be written in quotes
}
  \item{type}{
the data type of the file to export. Can be Robject, Rimage, excel, csv and textfile.
}
  \item{path}{
location of the file to export.
}
  \item{filename}{
name of the filename. By default, it will be the name of the R object.
}
  \item{date}{
a logical value indicating whether the filename must include the current date.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
df <- data.frame(a=rnorm(10), b=runif(10)) # Create a data frame object
uniexport(Robject="df", type = "csv", path = getwd(), date=T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
