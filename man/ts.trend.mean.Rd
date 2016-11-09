% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ts.trend.mean.R
\name{ts.trend.mean}
\alias{ts.trend.mean}
\title{Time Series Data Generator with Mean Trend}
\usage{
ts.trend.mean(N, TS, delta, tau, phi, theta, mu, sigma, seeds)
}
\arguments{
\item{N}{Number of time series}

\item{TS}{Size of the time series}

\item{delta}{Mean}

\item{tau}{Trend on the mean}

\item{phi}{Vector of autoregressive parameters}

\item{theta}{Vector of moving average parameters}

\item{mu}{Normal error mean}

\item{sigma}{Normal error standard deviation}

\item{seeds}{Vector of the seeds}
}
\value{
N time series of size TS
}
\description{
Generates non-stationary time series data with mean trend and
normal error distribution.
}
\examples{
ts.trend.mean(5, 5000, 0, 1, 0.9, 0, 0, 1, c(645,983,653,873,432))

}
