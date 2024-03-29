% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{get_z_hubs}
\alias{get_z_hubs}
\alias{get_y_hubs}
\title{Get most important hubs for each Z factor}
\usage{
get_z_hubs(fa, hubs_per_factor = 10, factors = 1:fa$rank)

get_y_hubs(fa, hubs_per_factor = 10, factors = 1:fa$rank)
}
\arguments{
\item{fa}{A \code{\link[=vsp_fa]{vsp_fa()}} object.}

\item{hubs_per_factor}{The number of important nodes to get per
latent factor. Defaults to \code{10}.}

\item{factors}{The specific columns to index into. The most reliable
option here is to index with an integer vector of column indices,
but you could also use a character vector if columns have been named.
By default returns all factors/singular vectors.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} where each row corresponds to a single
hub, and three columns:
\itemize{
\item \code{id}: Node id of hub node
\item \code{factor}: Which factor that node is a hub for. Nodes can be hubs
of multiple factors.
\item \code{loading}: The actual value of the hubs factor loading for that factor.
}
}
\description{
Get most important hubs for each Z factor
}
\section{Functions}{
\itemize{
\item \code{get_y_hubs()}: Get most important hubs for each Y factor

}}
\examples{

data(enron, package = "igraphdata")

fa <- vsp(enron, rank = 30)
fa

get_z_hubs(fa)
get_y_hubs(fa)

}
