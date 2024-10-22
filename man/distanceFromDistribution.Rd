% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/throwBall.R
\name{distanceFromDistribution}
\alias{distanceFromDistribution}
\title{Generate random distance from statistical distribution.}
\usage{
distanceFromDistribution(distribution, param1, param2)
}
\arguments{
\item{distribution}{string with distribution name, among:
'normal', 'uniform', 'poisson', 'bernoulli', 'binomial',
'geometric', 'degenerate', 'chisq', 'weibull'.}

\item{param1}{Numeric with first parameter from the distribution.}

\item{param2}{Numeric with second parameter from the distribution.}
}
\value{
Numeric of length 1 with distance generated from the specified distribution.
}
\description{
Generate random distance from statistical distribution.
}
