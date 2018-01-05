\name{my.summary}
\alias{my.summary}
\title{My Summary}
\usage{
my.summary(x,showNA,digits...)
}
\arguments{
  \item{x}{Numerical or factor vector to summarise.}
  \item{showNA}{NA description. TRUE as default.}
  \item{digits}{Number of digits desired. NULL as default.}
  \item{...}{further arguments passed to or from other methods.}
}
\value{
A vector with a summary statistics for numerical variables and frequencies for categorical variables.
}
\description{
Basic summary statistics for numerical variables.
}
\examples{
data(iris)

## Numerical variable:
is.numeric(iris$Sepal.Length) # TRUE

my.summary(iris$Sepal.Length)

ms = my.summary(iris$Sepal.Length)

# Select mean value:
ms[names(ms)=="mean"]

## Factor variable:
is.factor(iris$Species) # TRUE

my.summary(iris$Species)

ms = my.summary(iris$Species)

# Select setosa frequency:
ms[names(ms)=="setosa"]

}
