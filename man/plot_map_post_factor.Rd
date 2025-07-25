% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{plot_map_post_factor}
\alias{plot_map_post_factor}
\title{Plot map posterior association}
\usage{
plot_map_post_factor(proj, extra.only = F, r = 1)
}
\arguments{
\item{proj}{\code{fastan::project}.}

\item{extra.only}{logical, \code{TRUE} if only the entries from the extra group should be plotted, \code{FALSE} otherwise.}

\item{r}{numeric, default = 1, radius of the dots.}
}
\value{
\code{ggplot2}/\code{gridExtra} object.
}
\description{
Useful for semi-confimatory models.
}
