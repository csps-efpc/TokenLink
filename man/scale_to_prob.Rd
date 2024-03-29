% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenify.R
\name{scale_to_prob}
\alias{scale_to_prob}
\title{Scales a vector from 1-priori_delta to priori_delta}
\usage{
scale_to_prob(x, priori_delta = PRIORI_DELTA)
}
\arguments{
\item{x}{vector of numbers}

\item{priori_delta}{how much to compress the 0->1 scale range default PRIORI_DELTA}
}
\value{
returns a vector
}
\description{
Scales a vector from 1-priori_delta to priori_delta
}
\examples{
scale_to_prob(c(10,9,7,5,34,2,1,0,-1), priori_delta = 0)

}
