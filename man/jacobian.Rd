\name{jacobian}
\alias{jacobian}
\title{jacobian function}
\usage{
jacobian(f, y, x, derivs = NULL)
}
\arguments{
\item{f}{A function}

\item{y}{The variables to compute the derivatives of (the dependent variable). For example: \emph{df/dx}}

\item{x}{The variables to which respect the variables are calcualted (the independent variable). For example: \emph{df/dx}}

\item{derivs}{optional input defining own functions which should be used. See \code{\link{d}()} for details.}

\item{num_functions}{optional input defining number of functions otherwise a squared matrix form is assumed.}

}
\value{
A function that computes the jacobi-matrix of f. Notably, it expects the dame arguments as the input function \emph{f}.
}
\description{
Creates a function that computes the jacobi-matrix of a function for one specific variable. Hereinafter the variable is called \emph{y}.
The derivative is calculated with respect to one of the arguments of the function. Subsequently, the variable is called \emph{x}.
The returned function can be called at any possible point of \emph{x}.
}
\details{
The function \emph{jacobian} is intended for using it for functions accepting vectors (in case of \emph{x}) and returns a vector (for \emph{y}). \cr
Mentionable, only integers are allowed for indexing the vectors. Moreover, only one element at the time can be changed. 
For instance, \emph{y[1]} is permitted. In contrast, \emph{y[1.5]} or \emph{y[variable]} will throw an error.\cr
As usually it is possible to define new variables. 
If \emph{x} and/or \emph{y} are found at the right side of the assignment operator the variable is replaced in all following lines.
See the example below: \cr
\verb{
  # Old code
  a <- x[1]
  b <- 3
  y[1] <- a*b 
  # New code
  b <- 3
  y[1] <- a*3
} 

Furthermore, it is possible to use \emph{if, else if, else} blocks within the function.
However, the dependent variable have to be located at the left side of the assignment operator.
This restriction is necessary as variables found in previous lines are replaced in the following lines.
\verb{
  # allowed code
  f <- function(x, t) {
     y <- numeric(2)
     y[1] <- 2*x[1]^3
     if(t < 3) {
        y[2] <- x[2]^2
     } else {
        y[2] <- x[2]^4
     }
    return(y)
  }

  # not allowed code
  f <- function(x, t) {
     y <- numeric(2)
     y[1] <- 2*x[1]^3
     a <- 0
     if(t < 3) {
        a <- x[2]^2
     } else {
        a <- x[2]^4
     }
     y[2] <- a
    return(y)
  }
  
}
}
\examples{
f <- function(x) {
   y <- numeric(2)
   y[1] <- x[1]^2 + sin(4)
   y[2] <- x[2]*7
   return(y)
}
jac <- dfdr::jacobian(f, y, x)
jac(c(1, 2))
}
