% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/changepoint.variance.test.R
\name{changepoint.variance.test}
\alias{changepoint.variance.test}
\title{Change Point Variance Stationarity Test}
\usage{
changepoint.variance.test(data, alpha, penalty, method)
}
\arguments{
\item{data}{Time series data}

\item{alpha}{Value of alpha for the statistics test}

\item{penalty}{"None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic",  "Manual" and "CROPS" penalties.}

\item{method}{"AMOC", "PELT", "SegNeigh" or "BinSeg"}
}
\value{
0: Non-Stationary, 1: Stationary, NA: Unable to test
}
\description{
Change Point variance changes stationarity test.
}
