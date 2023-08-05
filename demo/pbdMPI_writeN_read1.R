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
data_local <- data_global[,rank*nc_loc+c(1:nc_loc)]

### Parallel write
filename <- "writeN_read1.nc"
info.create()
ncid <- create.nc(filename, format="netcdf4", mpi_comm=comm.c2f(), mpi_info=info.c2f())
rdim <- dim.def.nc(ncid, "rows", nr)
cdim <- dim.def.nc(ncid, "cols", nc)
varid <- var.def.nc(ncid, "data", "NC_INT", c(rdim, cdim))
var.put.nc(ncid, varid, data_local, start=c(1,rank*nc_loc+1), count=c(nr,nc_loc))
close.nc(ncid)
info.free()
barrier()

### Serial read
if (rank==0) {
  ncid2 <- open.nc(filename)
  data_global2 <- var.get.nc(ncid2, "data")
  close.nc(ncid2)
}

### Check global data on rank 0
if (rank==0) {
  cat("data_global=\n")
  print(data_global)
  cat("data_global2=\n")
  print(data_global2)
  if (!isTRUE(all.equal(data_global, data_global2))) {
    comm.abort()
  }
}

finalize()

