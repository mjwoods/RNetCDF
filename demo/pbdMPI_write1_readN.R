### SHELL> mpiexec -np 2 Rscript --vanilla [...].R

library(pbdMPI, quiet = TRUE)
library(RNetCDF, quiet = TRUE)

### MPI parameters
init()
rank <- comm.rank()
size <- comm.size()

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
barrier()

### Parallel read
info.create()
ncid2 <- open.nc(filename, mpi_comm=comm.c2f(), mpi_info=info.c2f())
data_local <- var.get.nc(ncid2, "data", start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))
close.nc(ncid2)
info.free()

### Check local slab against global data
comm.cat("data_local =", data_local, "\n", all.rank=TRUE)
comm.cat("data_global =", data_global[,rank*nc_loc+c(1:nc_loc)], "\n", all.rank=TRUE)
barrier()
if (!isTRUE(all.equal(data_local, data_global[,rank*nc_loc+c(1:nc_loc)]))) {
  comm.abort()
}

finalize()

