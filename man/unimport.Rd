\name{unimport}
\alias{unimport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
unimport
}
\description{
Reads a file in (txt, csv, xlsx and sidiap) formats and creates a data frame from it.
}
\usage{
unimport(type, path, filename, exact_match = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{exact_match}{
%%     ~~Describe \code{exact_match} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (type, path, filename, exact_match = F, ...)
{
    if (type == "sidiap") {
        lf = list.files(path = path, recursive = T, pattern = ".txt")
        fn = paste0(path, "/", grep(filename, lf, value = T))
        aux = sidiap.import(fn, ...)
    }
    else {
        lf = list.files(path = path, recursive = T, pattern = paste0(".",
            type))
        if (exact_match) {
            fn = paste0(path, "/", grep(paste0("^", filename,
                ".", type, "$"), lf, value = T, ignore.case = T))
        }
        else {
            fn = paste0(path, "/", grep(filename, lf, value = T,
                ignore.case = T))
        }
        dots = list(...)
        if (length(fn) > 1L) {
            fnames = names(formals(file_choose))
            fn = do.call(file_choose, c(type, path, filename,
                dots[names(dots) \%in\% fnames]))
        }
        if (type == "rds") {
            aux = readRDS(fn)
        }
        else if (type == "R") {
            aux = source(fn)
        }
        else if (type \%in\% c("txt", "csv", "xlsx")) {
            aux = do.call(cat.import, c(normalizePath(fn), dots_f(list(xlsx = "read.xlsx",
                data.table = c("fread")), dots)))
        }
    }
    return(aux)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
