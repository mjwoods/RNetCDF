# RNetCDF-test-netcdf4.nc was produced by RNetCDF-test.R from RNetCDF_2.6-1.
# It contains variables and attributes in all data types supported by netcdf4.

test_that("print.nc() output matches snapshot", {
  expect_snapshot(
    print.nc(open.nc(test_path("data", "RNetCDF-test-netcdf4.nc")))
  )
})


test_that("read.nc() output matches canned data", {
  ncdata <- read.nc(open.nc(test_path("data", "RNetCDF-test-netcdf4.nc")), recursive=TRUE)
  rdata <- readRDS(test_path("data", "RNetCDF-test-netcdf4.rds"))
  expect_equal(ncdata, rdata)
})

