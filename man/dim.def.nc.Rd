\name{dim.def.nc}

\alias{dim.def.nc}

\title{Define a NetCDF Dimension}

\description{Define a new NetCDF dimension.}

\usage{dim.def.nc(ncfile, dimname, dimlength=1, unlim=FALSE)}

\arguments{
  \item{ncfile}{Object of class "\code{NetCDF}" which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{dimname}{Dimension name. Must begin with an alphabetic character, followed by zero or more alphanumeric characters including the underscore ("\code{_}"). Case is significant.}
  \item{dimlength}{Length of dimension, that is, number of values for this dimension as an index to variables that use it. This must be a positive integer. If an unlimited dimension is created (\code{unlim=TRUE}), the value of \code{length} is not used.}
  \item{unlim}{Set to \code{TRUE} if an unlimited dimension should be created, otherwise to \code{FALSE}.}
}

\value{NetCDF variable identifier, returned invisibly.}

\details{This function creates a new NetCDF dimension. There is a suggested limit (100) to the number of dimensions. Ordinarily, the name and length of a dimension are fixed when the dimension is first defined. The name may be changed later, but the length of a dimension (other than the unlimited dimension) cannot be changed without copying all the data to a new NetCDF dataset with a redefined dimension length. A NetCDF dimension in an open NetCDF dataset is referred to by a small integer called a dimension ID. In the C interface, dimension IDs are 0, 1, 2, ..., in the order in which the dimensions were defined. At most one unlimited length dimension may be defined for each NetCDF dataset.}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a new NetCDF dataset and define two dimensions
file1 <- tempfile("dim.def_", fileext=".nc")
nc <- create.nc(file1)

dim.def.nc(nc, "station", 5)
dim.def.nc(nc, "time", unlim=TRUE)

close.nc(nc)
unlink(file1)
}

\keyword{file}
