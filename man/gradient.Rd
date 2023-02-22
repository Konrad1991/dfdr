\name{gradient}
\alias{gradient}
\title{Compute the gradient-function of a function.}
\usage{
gradient(f, use_names, ...)
}
\arguments{
\item{f}{A function}

\item{use_names}{Should the gradient add variable names to the output of the function?}

\item{...}{The variable names for which gradients should be calculated}
}
\value{
A function that computes the gradient of f at any point.
}
\description{
Creates a function that computes the derivative of a function with respect to each parameter
and return a vector of these.
}
\examples{
f <- function(x, y) x^2 + y^2
df <- gradient(f, FALSE, x, y)
df(1, 1)
}
