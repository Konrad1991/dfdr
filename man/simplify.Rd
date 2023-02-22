\name{simplify}
\alias{simplify}
\title{Simplify an expression by computing the values for constant expressions}
\usage{
simplify(expr)
}
\arguments{
\item{expr}{An expression}
}
\value{
a simplified expression
}
\description{
Simplify an expression by computing the values for constant expressions
}
\examples{
ex <- quote(a*0 + b^2 + 0)
simplify(ex)
}
