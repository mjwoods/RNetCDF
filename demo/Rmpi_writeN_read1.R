### SHELL> mpiexec -np 2 Rscript --vanilla [...].R

library(Rmpi, quiet = TRUE)
library(RNetCDF, quiet = TRUE)

### MPI parameters
comm <- 0
rank <- mpi.comm.rank(comm)
size <- mpi.comm.size(comm)

### Define global dimensions and data
nr <- 5
nc_loc <- 4
nc <- nc_loc * size
data_global <- matrix(seq(1,nc*nr), nrow=nr, ncol=nc)
data_local <- data_global[,rank*nc_loc+c(1:nc_loc)]

### Parallel write
filename <- "writeN_read1.nc"
ncid <- create.nc(filename, format="netcdf4", mpi_comm=mpi.comm.c2f(comm), mpi_info=NULL)
rdim <- dim.def.nc(ncid, "rows", nr)
cdim <- dim.def.nc(ncid, "cols", nc)
varid <- var.def.nc(ncid, "data", "NC_INT", c(rdim, cdim))
var.put.nc(ncid, varid, data_local, start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))
close.nc(ncid)
invisible(mpi.barrier(comm))

### Serial read
if (rank==0) {
  ncid2 <- open.nc(filename)
  data_global2 <- var.get.nc(ncid2, "data")
  close.nc(ncid2)
  unlink(filename)
}

### Check global data on rank 0
if (rank==0) {
  cat("data_global=\n")
  print(data_global)
  cat("data_global2=\n")
  print(data_global2)
  if (!isTRUE(all.equal(data_global, data_global2))) {
    mpi.abort(comm)
  }
}

invisible(mpi.finalize())

