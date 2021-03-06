% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts.stationarity.R
\name{ts.stationarity}
\alias{ts.stationarity}
\title{Time Series Data Generator with Normal Error Distribution}
\usage{
ts.stationarity(N, TS, delta, phi, theta, error, seeds, burnin)
}
\arguments{
\item{N}{Number of time series}

\item{TS}{Size of the time series}

\item{delta}{Drift constant}

\item{phi}{Vector of autoregressive parameters}

\item{theta}{Vector of moving average parameters}

\item{error}{Type of error and parameters
Normal      - c(ERROR_N, mean, stdv)
Exponential - c(ERROR_E, mean, lambda)
Triangle    - c(ERROR_T, lower, upper, mode)}

\item{seeds}{Vector of seeds}

\item{burnin}{Number of samples thrown away at the beginning of time series generation}
}
\value{
N time series of size TS
}
\description{
Generates non-stationary time series data with normal error distribution.
}
\examples{
ts.stationarity(5, 5000, 1, 0.9, 0, c(ERROR_N,0,1), c(645,983,653,873,432), 10)

}
