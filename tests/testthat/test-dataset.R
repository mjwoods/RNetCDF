for (format in c("classic","offset64","data64","classic4","netcdf4")) {

  test_that(paste("dataset creation in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- skip_unsupported(create.nc(ncfile, clobber=TRUE, format=format))
    expect_s3_class(nc, "NetCDF", exact=TRUE)

    # Check file format reported by library:
    inq <- file.inq.nc(nc)
    expect_equal(inq$format, format)
  })

}


test_that("file.inq.nc returns named list", {
  ncfile <- withr::local_tempfile(lines="")
  nc <- create.nc(ncfile, clobber=TRUE)

  inq <- file.inq.nc(nc)
  expect_type(inq, "list")
  expect_length(inq, 6)
  expect_equal(inq$ndims, 0)
  expect_equal(inq$nvars, 0)
  expect_equal(inq$ngatts, 0)
  expect_equal(inq$unlimdimid, NA_integer_)
  expect_equal(inq$format, "classic")
  expect_type(inq$libvers, "character")
  message("INFO: NetCDF library: ", inq$libvers)
})


test_that("sync.nc returns invisible null", {
  ncfile <- withr::local_tempfile(lines="")
  nc <- create.nc(ncfile, clobber=TRUE)
  
  sync <- expect_invisible(sync.nc(nc))
  expect_null(sync)
})


test_that("open.nc returns NetCDF object", {
  ncfile <- withr::local_tempfile(lines="")
  nc <- create.nc(ncfile, clobber=TRUE, share=TRUE)
  close.nc(nc)
  nc2 <- open.nc(ncfile)
  expect_s3_class(nc2, "NetCDF", exact=TRUE)
})


test_that("close.nc closes copies of NetCDF object", {
  ncfile <- withr::local_tempfile(lines="")
  nc <- create.nc(ncfile, clobber=TRUE)
  nc2 <- nc
  expect_no_error(file.inq.nc(nc2))
  close.nc(nc)
  expect_error(file.inq.nc(nc))
  expect_error(file.inq.nc(nc2))
})


test_that("gc closes unreferenced NetCDF object", {
  ncfile <- withr::local_tempfile(lines="")
  nc <- create.nc(ncfile, clobber=TRUE)
  # For testing only; users should not modify NetCDF objects:
  handle_ptr <- attr(nc, "handle_ptr")
  attr(nc, "handle_ptr") <- NULL
  expect_no_error(file.inq.nc(nc))
  rm(handle_ptr)
  gc()
  expect_error(file.inq.nc(nc))
})


test_that("gc preserves duplicate NetCDF object", {
  ncfile <- withr::local_tempfile(lines="")
  nc <- create.nc(ncfile, clobber=TRUE)
  nc2 <- nc
  expect_no_error(file.inq.nc(nc2))
  rm(nc)
  gc()
  expect_no_error(file.inq.nc(nc2))
})


for (format in c("classic","netcdf4")) {

  test_that(paste("create.nc refuses to overwrite files by default in", format, "format"), {
    ncfile <- withr::local_tempfile(lines="")
    nc <- expect_error(create.nc(ncfile, clobber=FALSE))
  })


  test_that(paste("create.nc allows diskless mode in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- skip_unsupported(create.nc(ncfile, clobber=TRUE, format=format, diskless=TRUE))
    close.nc(nc)
    expect_error(open.nc(ncfile))
  })


  test_that(paste("create.nc allows diskless, persistent mode in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- skip_unsupported(create.nc(ncfile, clobber=TRUE, format=format, diskless=TRUE, persist=TRUE))
    close.nc(nc)
    expect_no_error(open.nc(ncfile))
  })


  test_that(paste("sync.nc flushes file content in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- create.nc(ncfile, clobber=TRUE, format=format)
    att.put.nc(nc, "NC_GLOBAL", "sync", "NC_CHAR", "should flush")
    sync.nc(nc)
    nc2 <- open.nc(ncfile)
    expect_no_error(att.get.nc(nc2, "NC_GLOBAL", "sync"))
  })


  test_that(paste("create.nc prefill is enabled by default in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- create.nc(ncfile, clobber=TRUE, format=format)
    nx <- 100
    dim.def.nc(nc, "nx", nx)
    var.def.nc(nc, "prefill", "NC_INT", "nx")
    att.put.nc(nc, "prefill", "_FillValue", "NC_INT", -1)
    var.put.nc(nc, "prefill", 0, start=1, count=1)
    actual <- var.get.nc(nc, "prefill", na.mode=3)
    expect <- array(c(0, rep(-1, times=nx-1)), nx)
    expect_equal(actual, expect)
  })


  test_that(paste("open.nc is read-only by default in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- create.nc(ncfile, clobber=TRUE, format=format)
    close.nc(nc)
    nc2 <- open.nc(ncfile)
    expect_error(att.put.nc(nc2, "NC_GLOBAL", "open.nc", "NC_CHAR", "read-only"))
  })


  test_that(paste("open.nc prefill is enabled by default in", format, "format"), { 
    ncfile <- withr::local_tempfile(lines="")
    nc <- create.nc(ncfile, clobber=TRUE, format=format)
    close.nc(nc)
    nc2 <- open.nc(ncfile, write=TRUE)
    nx <- 100
    dim.def.nc(nc2, "nx", nx)
    var.def.nc(nc2, "prefill", "NC_INT", "nx")
    att.put.nc(nc2, "prefill", "_FillValue", "NC_INT", -1)
    var.put.nc(nc2, "prefill", 0, start=1, count=1)
    actual <- var.get.nc(nc2, "prefill", na.mode=3)
    expect <- array(c(0, rep(-1, times=nx-1)), nx)
    expect_equal(actual, expect)
  })


  test_that(paste("open.nc allows diskless mode in", format, "format"), {
    ncfile <- withr::local_tempfile(lines="")
    nc <- create.nc(ncfile, clobber=TRUE, format=format)
    close.nc(nc)
    nc2 <- skip_unsupported(open.nc(ncfile, write=TRUE, diskless=TRUE))
    expect_s3_class(nc2, "NetCDF")
  })


  test_that(paste("open.nc allows diskless, persistent mode in", format, "format"), {
    ncfile <- withr::local_tempfile(lines="")
    nc <- create.nc(ncfile, clobber=TRUE, format=format)
    close.nc(nc)
    nc2 <- skip_unsupported(open.nc(ncfile, write=TRUE, diskless=TRUE, persistent=TRUE))
    expect_s3_class(nc2, "NetCDF")
  })

}


test_that("create.nc supports MPI in netcdf4 format", {
    skip_not_pbdMPI()
    rank <- pbdMPI::comm.rank()
    comm <- pbdMPI::comm.c2f()
    pbdMPI::info.create()
    info <- pbdMPI::info.c2f()
    if (rank == 0) {
      ncfile <- withr::local_tempfile(lines="")
    }
    ncfile <- pbdMPI::bcast(ncfile, 0)
    
    nc <- skip_unsupported(create.nc(ncfile, clobber=TRUE, format=format, mpi_comm=comm, mpi_info=info))
    expect_s3_class(nc, "NetCDF") 
  })

test_that("open.nc supports MPI in netcdf4 format", {
    skip_not_pbdMPI()
    rank <- pbdMPI::comm.rank()
    comm <- pbdMPI::comm.c2f()
    pbdMPI::info.create()
    info <- pbdMPI::info.c2f()
    if (rank == 0) {
      ncfile <- withr::local_tempfile(lines="")
      nc <- create.nc(ncfile, clobber=TRUE, format=format)
      close.nc(nc)
    }
    ncfile <- pbdMPI::bcast(ncfile, 0)
    
    nc2 <- skip_unsupported(open.nc(ncfile, write=TRUE, mpi_comm=comm, mpi_info=info))
    expect_s3_class(nc2, "NetCDF") 
  })

