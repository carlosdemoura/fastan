% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_process.R
\name{real_from_dist}
\alias{real_from_dist}
\title{Generate real value list from distributions}
\usage{
real_from_dist(group.sizes, columns, semi.conf = NULL, dist = list())
}
\arguments{
\item{group.sizes}{integer vector with size of each group.}

\item{columns}{integer, number of columns.}

\item{semi.conf}{logical, \code{TRUE} if the model is semi-confirmatory, \code{FALSE} otherwise, NULL (default) means the code will infer the model is not semi.conf if the number of groups is <= 2}

\item{dist}{list of distributions from \code{distributional} from which the parameters will be sampled.}
}
\value{
real value list.
}
\description{
Used on \code{fastan::generate_data}
}
