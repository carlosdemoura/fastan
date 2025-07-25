% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/space.R
\name{generate_space}
\alias{generate_space}
\title{Generate random points}
\usage{
generate_space(n, cont = F)
}
\arguments{
\item{n}{integer, number of points to be generated.}

\item{cont}{logical, default = \code{FALSE}, if \code{TRUE} orders the random coordinates by longitude (returning contiguous groups), if \code{FALSE} does nothing.}
}
\value{
data.frame.
}
\description{
Generate random points
}
