\name{ORsummary}
\alias{ORsummary}
\title{ORsummary}
\description{
Prepares the Odds Ratio information from the \code{oddsratio} function of the \code{epitools} package and print it in export-ready format.}
\usage{
ORsummary(x)
}
\arguments{
  \item{x}{input data can be one of the following: r x 2 table, vector of numbers from a contigency table (will be transformed into r x 2 table in row-wise order), or single factor or character vector that will be combined with y into a table.}

  \item{...}{further arguments passed to or from other methods.}
}

\value{
A matrix with a summary statistics. Specifically the function returns the percentatges of the contigency table, the marginal frecuency, the odds ratio and the p values.
}

\examples{
data(infert)

ORsummary(table(infert$education,infert$case))
ORsummary(infert$education,infert$case) ## another possible format

}
