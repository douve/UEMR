\name{cat.import}
\alias{cat.import}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reads and returns text and excel files
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cat.import(file, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
full file path
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
function (file, ...)
{
    type = stringr::str_extract(file, "[^.]*$")
    if (type == "xlsx") {
        l <- dots_f(list(readxl = "read_excel"), list(...))
        if (is.null(l$sheet))
            l$sheet = 1
        l = c(list(file), l)
        out = data.table::setDT(do.call(readxl::read_excel, l))
    }
    else {
        l <- dots_f(list(data.table = "fread"), list(...))
        l = c(list(file), l)
        out = do.call(data.table::fread, l)
    }
    out[out == ""] <- NA
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
