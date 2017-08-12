# Build against imagemagick static website.
VERSION <- commandArgs(TRUE)
if(!file.exists(sprintf("../windows/netcdf-%s/include/netcdf.h", VERSION))){
  if(getRversion() < "3.3.0") setInternet2()
  download.file(sprintf("https://github.com/rwinlib/netcdf/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
