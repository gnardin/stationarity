# stationarity R package

<h2>Build Documentation</h2>
$ R

> setwd(\<stationarity source-code package\>)

> devtools::document()

<h2>Compile Package</h2>
$ R CMD build stationarity

$ R CMD check stationarity_0.1.0.tar.gz

<h2>Install Package</h2>
$ R CMD INSTALL -l _\<packages path\>_ stationarity_0.1.0.tar.gz

where, _\<packages path\>_ is the directory where to install the stationarity package
