\name{config.nc}

\alias{config.nc}

\title{Configured options}

\description{Find NetCDF options detected when installing RNetCDF.}

\usage{config.nc()}

\details{This function is not intended for user code, and it is subject to change or removal without notice. It is currently needed for RNetCDF package tests, to determine expected behaviour of the NetCDF C library.

Unless otherwise documented, optional NetCDF features that are not detected when installing RNetCDF will raise an error when called from R code. If necessary, work arounds can be implemented by wrapping the relevant code in \code{try} or \code{tryCatch}.
}

\author{Pavel Michna, Milton Woods}

\keyword{utilities}
