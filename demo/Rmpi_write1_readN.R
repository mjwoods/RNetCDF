### SHELL> mpiexec -np 2 Rscript --vanilla [...].R

library(Rmpi, quiet = TRUE)
library(RNetCDF, quiet = TRUE)

### MPI parameters
rank <- mpi.comm.rank()
size <- mpi.comm.size()

### Define global dimensions and data
nr <- 5
nc_loc <- 4
nc <- nc_loc * size
data_global <- matrix(seq(1,nc*nr), nrow=nr, ncol=nc)

### Serial write
filename <- "write1_readN.nc"
if (rank==0) {
  ncid <- create.nc(filename, format="netcdf4")
  rdim <- dim.def.nc(ncid, "rows", nr)
  cdim <- dim.def.nc(ncid, "cols", nc)
  varid <- var.def.nc(ncid, "data", "NC_INT", c(rdim, cdim))
  var.put.nc(ncid, varid, data_global)
  close.nc(ncid)
}
mpi.barrier()

### Parallel read
ncid2 <- open.nc(filename, mpi_comm=comm.c2f(), mpi_info=NULL)
data_local <- var.get.nc(ncid2, "data", start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))
close.nc(ncid2)

### Check local slab against global data
mpi.barrier()
if (!isTRUE(all.equal(data_local, data_global[,rank*nc_loc+c(1:nc_loc)]))) {
  mpi.abort()
}

mpi.finalize()

