% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior.R
\name{get_chains_mcmc}
\alias{get_chains_mcmc}
\title{Transform \code{rstan} fit into \code{coda::mcmc.list()}}
\usage{
get_chains_mcmc(fit, param)
}
\arguments{
\item{fit}{\code{rstan::stan()} object.}

\item{param}{string; must be in rstan::stan fit format.}
}
\value{
\code{coda::mcmc.list()}
}
\description{
Transform \code{rstan} fit into \code{coda::mcmc.list()}
}
