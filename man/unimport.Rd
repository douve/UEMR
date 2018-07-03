\name{unimport}
\alias{unimport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
unimport
}
\description{
Reads a file in 6 data types (Rscript, Rimage, data.frame, excel, csv, sidiap and textfile) and creates a data frame from it.
}
\usage{
unimport(type, path, filename, exact_string = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
The data type of the file to import. Can be Rscript, Rimage, data.frame, excel, csv, sidiap and textfile.
}
  \item{path}{
Location of the file to import.
}
  \item{filename}{
Name of the filename. Can be a substring of this.
}
  \item{exact_match}{
default is FALSE. If TRUE then the files will be searched for that have exactly the requested file name and no more characters.
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
df123 <- data.frame(a=rnorm(10), b=runif(10)) # Create a data frame object
saveRDS(df123,paste0(getwd(),"/df123.rds")) # we save it on our current directory
# we load it with unimport
unimport(type = "data.frame", path = getwd(), filename = "df")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
