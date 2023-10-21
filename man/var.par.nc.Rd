\name{var.par.nc}

\alias{var.par.nc}

\title{Change Parallel Access Mode}

\description{Change the parallel access mode of a NetCDF variable from independent to collective and vice versa.}

\usage{var.par.nc(ncfile, variable, access="NC_COLLECTIVE")}

\arguments{
  \item{ncfile}{Object of class \code{NetCDF} which points to the NetCDF dataset (as returned from \code{\link[RNetCDF]{open.nc}}).}
  \item{variable}{Numeric ID or name of the variable for which to change the parallel access mode. Use \code{"NC_GLOBAL"} to change the parallel access mode for all variables in the dataset.}
  \item{access}{Parallel access mode as one of the following strings: \code{"NC_COLLECTIVE"} or \code{"NC_INDEPENDENT"}.}
}

\details{Parallel file access is either collective (all processors must participate) or independent (any processor may access the data without waiting for others). Data reads and writes (i.e. calls to \code{var.put.nc} and \code{var.get.nc}) are independent by default. Use this function to change the parallel access mode for a variable from independent to collective mode or vice versa.

All netCDF metadata writing operations are collective - all creation of groups, types, variables, dimensions, or attributes.

Note that when the file format is \code{"classic"} or \code{"offset64"}, the change always applies to all variables in the file, even if a single variable is specified in argument \code{variable}.}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{\dontrun{
# This example assumes that the NetCDF library was built with MPI support,
# and that both RNetCDF and pbdMPI are installed in R.
# If the example code is stored in a file myexample.R,
# run R under MPI using a command similar to:
# SHELL> mpiexec -np 2 Rscript --vanilla myexample.R

library(pbdMPI, quiet = TRUE)
library(RNetCDF, quiet = TRUE)

# Get MPI parameters
init()
rank <- comm.rank()
size <- comm.size()

# Define dimensions and data
nr <- 5
nc_local <- 4
nc <- nc_local * size
data_local <- matrix(rank, nrow=nr, ncol=nc_local)

# Open file for parallel access and define metadata
filename <- "myexample.nc"
info.create()
ncid <- create.nc(filename, format="netcdf4", mpi_comm=comm.c2f(), mpi_info=info.c2f())
rdim <- dim.def.nc(ncid, "rows", nr)
cdim <- dim.def.nc(ncid, "cols", nc)
varid <- var.def.nc(ncid, "data", "NC_INT", c(rdim, cdim))

# Use collective I/O
var.par.nc(ncid, "data", "NC_COLLECTIVE")

# Write data
var.put.nc(ncid, varid, data_local, start=c(1,rank*nc_local+1), count=c(nr,nc_local))

# Finish up
close.nc(ncid)
info.free()

finalize()
}}

\keyword{file}
