\name{uemFormatC}
\alias{uemFormatC}
\title{uemFormatC}
\description{
Computes export-ready format for a character vector of frecuencies, p-values and Odds Ratios.
}
\usage{
uemFormatC(x)
}
\arguments{
  \item{x}{input data can be a character vector of integers, p-values or an Odds Ratio point estimation.}
  \item{estimate}{Use in case \code{x} is an Odds Ratio.}

  \item{...}{further arguments passed to or from other methods.}
}

\value{
A character vector.
}

\examples{

## p-values

p.values <- c(.1,.5,.99,1)
uemFormatC(p.values)

## Frequency

uemFormatC(1:5, digits=3)

## Odds Ratio

OR <- 2
lower.limit <- 1.5
upper.limit <- 3.8

uemFormatC(OR, lower = lower.limit, upper = upper.limit, estimate = T)

}
