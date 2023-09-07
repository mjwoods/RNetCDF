### SHELL> mpiexec -np 2 Rscript --vanilla [...].R filename.nc

library(pbdMPI, quiet = TRUE)
library(RNetCDF, quiet = TRUE)

testfun <- function(x,y) {
  if (isTRUE(all.equal(x,y))) {
    cat("OK\n")
    return(TRUE)
  } else {
    cat("Failed\n")
    cat("x:\n")
    print(x)
    cat("y:\n")
    print(y)
    return(FALSE)
  }
}

### MPI parameters
init()
rank <- comm.rank()
size <- comm.size()

### Define global dimensions and data
nr <- 5
nc_loc <- 4
nc <- nc_loc * size
data_global <- matrix(seq(1,nc*nr), nrow=nr, ncol=nc)
data_local <- data_global[,rank*nc_loc+c(1:nc_loc)]

### Parallel write
args <- commandArgs(TRUE)
stopifnot(length(args) == 1)
filename <- args[1]

info.create()
ncid <- create.nc(filename, format="netcdf4", mpi_comm=comm.c2f(), mpi_info=info.c2f())
rdim <- dim.def.nc(ncid, "rows", nr)
cdim <- dim.def.nc(ncid, "cols", nc)

indid <- var.def.nc(ncid, "independent", "NC_INT", c(rdim, cdim))
var.par.nc(ncid, indid, "NC_INDEPENDENT") # Default mode, but make explicit here.
colid <- var.def.nc(ncid, "collective", "NC_INT", c(rdim, cdim))
var.par.nc(ncid, colid, "NC_COLLECTIVE")

var.put.nc(ncid, indid, data_local, start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))
var.put.nc(ncid, colid, data_local, start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))

close.nc(ncid)
info.free()
barrier()

### Serial read
if (rank==0) {
  ncid2 <- open.nc(filename)
  data_ind <- var.get.nc(ncid2, "independent")
  data_col <- var.get.nc(ncid2, "collective")
  close.nc(ncid2)
  unlink(filename)
}

### Check global data on rank 0
if (rank==0) {
  cat("Checking independent parallel write with pbdMPI ... ")
  ind_ok <- testfun(data_global, data_ind)

  cat("Checking collective parallel write with pbdMPI ... ")
  col_ok <- testfun(data_global, data_col)

  if (!ind_ok || !col_ok) {
    comm.abort()
  }
}

finalize()

