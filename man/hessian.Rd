\name{hessian}
\alias{hessian}
\title{Compute the Hessian-function of a function.}
\usage{
hessian(f, use_names = FALSE, ...)
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
Creates a function that computes the second-order derivatives of a function with respect to each pair of parameters
and return a vector of these.
}
\examples{
f <- function(x, y) x**2 + y**2
h <- hessian(f, FALSE, x, y)
h(0, 0)
}
