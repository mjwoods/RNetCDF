\name{open.nc}

\alias{open.nc}

\title{Open a NetCDF Dataset}

\description{Open an existing NetCDF dataset for reading and (optionally) writing.}

\usage{
   open.nc(con, write=FALSE, share=FALSE, prefill=TRUE, diskless=FALSE, persist=FALSE,
           mpi_comm=NULL, mpi_info=NULL, ...)
}

\arguments{
  \item{con}{Filename of the NetCDF dataset to be opened. If the underlying NetCDF library supports OPeNDAP, \code{con} may be an OPeNDAP URL.}
  \item{write}{If \code{FALSE} (default), the dataset will be opened read-only. If \code{TRUE}, the dataset will be opened read-write.}
  \item{share}{The buffer scheme. If \code{FALSE} (default), dataset access is buffered and cached for performance. However, if one or more processes may be reading while another process is writing the dataset, set to \code{TRUE}.}
  \item{prefill}{The prefill mode. If \code{TRUE} (default), newly defined variables are initialised with fill values when they are first accessed. This allows unwritten array elements to be detected when reading, but it also implies duplicate writes if all elements are subsequently written with user-specified data. Enhanced write performance can be obtained by setting \code{prefill=FALSE}.}
  \item{diskless}{When \code{diskless=TRUE}, the file is read entirely into memory, and any changes are kept in memory without writing to disk. The netcdf library may ignore this option for files in netcdf4 format.}
  \item{persist}{When \code{persist=TRUE}, a file opened with \code{diskless=TRUE} is flushed to disk when closed. In some cases, this may be faster than manipulating files directly on disk.}
  \item{mpi_comm}{Fortran handle of MPI communicator for parallel I/O. The default of \code{NULL} implies serial I/O. Valid Fortran handles may be obtained from your chosen MPI package for R - for example \link[pbdMPI]{comm.c2f} or \link[Rmpi]{mpi.comm.c2f}.}
  \item{mpi_info}{Fortran handle of MPI Info object for parallel I/O. The default value \code{NULL} specifies that the \code{MPI_INFO_NULL} object is used. Other valid Fortran handles may be obtained from your chosen MPI package for R - for example \link[pbdMPI]{info.c2f}.}
  \item{...}{Arguments passed to or from other methods (not used).}
}

\value{Object of class \code{NetCDF} which points to the NetCDF dataset, returned invisibly.}

\details{This function opens an existing NetCDF dataset for access. By default, the dataset is opened read-only. If \code{write=TRUE}, then the dataset can be changed. This includes appending or changing data, adding dimensions, variables, and attributes.}

\references{\url{https://www.unidata.ucar.edu/software/netcdf/}}

\author{Pavel Michna, Milton Woods}

\examples{
##  Create a void NetCDF dataset
file1 <- tempfile("open_", fileext=".nc")
nc <- create.nc(file1)
close.nc(nc)

##  Open the NetCDF dataset for writing
nc <- open.nc(file1, write=TRUE)
close.nc(nc)
unlink(file1)
}

\keyword{file}
