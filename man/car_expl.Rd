% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prior_engines.R
\name{car_expl}
\alias{car_expl}
\title{prior engine: CAR for alpha exploratory model}
\usage{
car_expl(proj, neib, tau)
}
\arguments{
\item{proj}{\code{fastan::project} object.}

\item{neib}{function that calculates the neighborhood matrix.}

\item{tau}{numeric, to scale CAR matrix.}
}
\value{
list of one covariance matrix.
}
\description{
prior engine: CAR for alpha exploratory model
}
