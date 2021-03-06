% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{safe_row_l2_normalize}
\alias{safe_row_l2_normalize}
\title{Safe L2 row normalization}
\usage{
safe_row_l2_normalize(x, eps = 1e-10)
}
\arguments{
\item{x}{A matrix to row normalize.}

\item{eps}{Tolerance to use when assessing if squared L2 row norm is
numerically larger or smaller than zero.}
}
\value{
The row-rescaled matrix
}
\description{
Helper function for Kaiser normalization to handle rows with zero (or
numerically zero) norm, which results in a divide by zero error
in the \code{stats::varimax()} implementation.
}
\keyword{internal}
