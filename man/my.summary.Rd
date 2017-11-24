\name{my.summary}
\alias{my.summary}
\title{My Summary}
\usage{
my.summary(x,...)
}
\arguments{
  \item{x}{Numerical vector to summarise.}

  \item{...}{further arguments passed to or from other methods.}
}
\value{
A vector with a summary statistics for numerical variables. Specifically the function returns the mean, sd, median, min, max and number of missings NA in the R object.
}
\description{
Basic summary statistics for numerical variables.
}
\examples{
data(iris)
is.numeric(iris$Sepal.Length) ## TRUE

my.summary(iris$Sepal.Length)

ms = my.summary(iris$Sepal.Length)

# Select mean value:
ms[names(ms)=="mean"]
}
