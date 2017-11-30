#===============================================================================#
#
#  Name:       RNetCDF-test.R
#
#  Version:    2.0-1
#
#  Purpose:    Test functions to the NetCDF interface for R.
#
#  Author:     Pavel Michna (rnetcdf-devel@bluewin.ch)
#              Milton Woods (miltonjwoods@gmail.com)
#
#  Copyright:  (C) 2010-2016 Pavel Michna
#
#===============================================================================#
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#===============================================================================#
#  Implementation and Revisions
#-------------------------------------------------------------------------------#
#  Author   Date       Description
#  ------   ----       -----------
#  pm       29/12/10   First implementation
#  mw       18/07/12   Test packed variables
#  mw       02/09/14   Test 1D character arrays and character scalars
#  mw       05/09/14   Test reading/writing NC_CHAR as raw bytes
#  mw       26/01/16   Test utcal.nc and utinvcal.nc with POSIXct type
#  mw       13/02/16   Test file operations in all supported on-disk formats
#
#===============================================================================#


#===============================================================================#
#  Load library
#===============================================================================#

library(RNetCDF)


#===============================================================================#
#  Run tests
#===============================================================================#

#-------------------------------------------------------------------------------#
#  NetCDF library functions
#-------------------------------------------------------------------------------#

#--Initialize ------------------------------------------------------------------#
cat("Starting NetCDF tests...\n")

testfun <- function(x,y,tally=NULL) {
  if (is.null(tally)) {
    tally <- c(pass=0,fail=0)
  }
  # Compare numeric values with single precision tolerance:
  if (isTRUE(all.equal(x,y,tolerance=2^(-23)))) {
    cat("OK\n")
    return(tally+c(1,0))
  } else {
    cat("Failed\n")
    cat("x:\n")
    print(x)
    cat("y:\n")
    print(y)
    return(tally+c(0,1))
  }
}

tally <- NULL

##  Create a new NetCDF dataset and define dimensions
for (format in c("classic","offset64","classic4","netcdf4")) {
  cat("Test",format,"file format ...\n")

  ncfile <- paste("test_", format, ".nc", sep="")
  nc <- create.nc(ncfile, format=format)

  nstation <- 5
  ntime <- 2
  nstring <- 32
  nempty <- 0

  dim.def.nc(nc, "station", nstation)
  dim.def.nc(nc, "time", ntime)
  dim.def.nc(nc, "max_string_length", nstring)
  dim.def.nc(nc, "empty", unlim=TRUE)

  ## Define a group
  if (format == "netcdf4") {
    ncroot <- nc
    nc <- grp.def.nc(nc, "testgrp")
  }

  ##  Define variables
  var.def.nc(nc, "time", "NC_INT", "time")
  var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1))
  var.def.nc(nc, "packvar", "NC_BYTE", c("station"))
  var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))
  var.def.nc(nc, "qcflag", "NC_CHAR", c("station"))
  var.def.nc(nc, "int0", "NC_INT", NA)
  var.def.nc(nc, "char0", "NC_CHAR", NA)
  var.def.nc(nc, "numempty", "NC_FLOAT", c("station","empty"))

  if (format == "netcdf4") {
    var.def.nc(nc, "namestr", "NC_STRING", c("station"))
    var.def.nc(nc, "stationid", "NC_UINT64", c("station"))
  }

  ##  Put some missing_value attribute for temperature
  att.put.nc(nc, "temperature", "missing_value", "NC_DOUBLE", -99999.9)

  ## Define the packing used by packvar
  att.put.nc(nc, "packvar", "scale_factor", "NC_DOUBLE", 10)
  att.put.nc(nc, "packvar", "add_offset", "NC_DOUBLE", -5)

  ## Define some additional test attributes:
  att_text <- "This is some text"
  att_text2 <- c("This is string 1", "This is string 2")
  hugeint <- "-1234567890123456789"
  att.put.nc(nc, "NC_GLOBAL", "char_att", "NC_CHAR", att_text)
  att.put.nc(nc, "name", "char_att", "NC_CHAR", att_text)
  att.put.nc(nc, "name", "raw_att", "NC_CHAR", charToRaw(att_text))
  if (format == "netcdf4") {
    att.put.nc(nc, "temperature", "string_att", "NC_STRING", att_text2)
    att.put.nc(nc, "temperature", "int64_att", "NC_INT64", hugeint)
  }

  ##  Define variable values
  mytime        <- c(1:2)
  mytemperature <- matrix(c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9),ncol=ntime)
  mypackvar     <- seq_len(5)*10-5
  myname        <- c("alfa", "bravo", "charlie", "delta", "echo")
  myid          <- paste("1234567890123456789",c("0","1","2","3","4"),sep="")
  myqcflag      <- "ABCDE"
  myint0        <- 12345
  mychar0       <- "?"

  ##  Put the data
  var.put.nc(nc, "time", mytime, 1, length(mytime))
  var.put.nc(nc, "temperature", mytemperature, c(1,1), c(nstation,ntime))
  var.put.nc(nc, "packvar", mypackvar, pack=TRUE)
  var.put.nc(nc, "name", myname, c(1,1), c(nstring,nstation))
  var.put.nc(nc, "qcflag", charToRaw(myqcflag))
  var.put.nc(nc, "int0", myint0)
  var.put.nc(nc, "char0", mychar0)

  if (format == "netcdf4") {
    var.put.nc(nc, "namestr", myname)
    var.put.nc(nc, "stationid", myid)
  }

  sync.nc(nc)

  ## Read tests

  cat("Read NC_CHAR global attribute ...")
  x <- att_text
  y <- att.get.nc(nc, "NC_GLOBAL", "char_att")
  tally <- testfun(x,y,tally)

  cat("Read NC_CHAR variable attribute ...")
  x <- att_text
  y <- att.get.nc(nc, "name", "char_att")
  tally <- testfun(x,y,tally)

  cat("Read NC_CHAR variable attribute as raw bytes ...")
  x <- charToRaw(att_text)
  y <- att.get.nc(nc, "name", "raw_att", rawchar=TRUE)
  tally <- testfun(x,y,tally)

  if (format == "netcdf4") {
    cat("Read NC_STRING variable attribute ...")
    x <- att_text2
    y <- att.get.nc(nc, "temperature", "string_att")
    tally <- testfun(x,y,tally)

    cat("Read NC_INT64 variable attribute as character ...")
    x <- hugeint
    y <- att.get.nc(nc, "temperature", "int64_att", fitnum=TRUE)
    tally <- testfun(x,y,tally)

    cat("Read NC_INT64 variable attribute as numeric ...")
    x <- as.numeric(hugeint)
    y <- att.get.nc(nc, "temperature", "int64_att")
    tally <- testfun(x,y,tally)
  }

  grpinfo <- grp.inq.nc(nc)
  cat("Inquire about groups in file/group ...")
  tally <- testfun(grpinfo$grps,list(),tally)
  cat("Inquire about dimension ids in file/group ...")
  tally <- testfun(grpinfo$dimids,c(0:3),tally)
  if (format == "netcdf4") {
    cat("Inquire about variable ids in file/group ...")
    tally <- testfun(grpinfo$varids,c(0:9),tally)
  } else {
    cat("Inquire about variable ids in file/group ...")
    tally <- testfun(grpinfo$varids,c(0:7),tally)
  }
  cat("Inquire about fullname of file/group ...")
  if (format == "netcdf4") {
    tally <- testfun(grpinfo$fullname,"/testgrp",tally)
  } else {
    tally <- testfun(grpinfo$fullname,"/",tally)
  }
  cat("Inquire about unlimited dimension ids of file/group ...")
  if (format == "netcdf4") {
    # Some versions of netcdf4 do not list unlimited dimensions in ancestor groups:
    if (length(grpinfo$unlimids)==0) {
      tally <- testfun(grpinfo$unlimids,integer(0),tally)
    } else {
      tally <- testfun(grpinfo$unlimids,3,tally)
    }
  } else {
    tally <- testfun(grpinfo$unlimids,3,tally)
  }

  cat("Read integer vector as double ... ")
  x <- mytime
  dim(x) <- length(x)
  y <- var.get.nc(nc, 0)
  tally <- testfun(x,y,tally)
  tally <- testfun(is.double(y),TRUE,tally)

  cat("Read integer vector as integer ... ")
  x <- mytime
  dim(x) <- length(x)
  y <- var.get.nc(nc, 0, fitnum=TRUE)
  tally <- testfun(x,y,tally)
  tally <- testfun(is.integer(y),TRUE,tally)

  cat("Read numeric matrix ... ")
  x <- mytemperature
  y <- var.get.nc(nc, "temperature")
  tally <- testfun(x,y,tally)

  cat("Read numeric matrix slice ... ")
  x <- mytemperature[,2]
  dim(x) <- length(x)
  y <- var.get.nc(nc, "temperature", c(NA,2), c(NA,1))
  tally <- testfun(x,y,tally)

  cat("Read numeric matrix empty slice ... ")
  x <- numeric(0)
  dim(x) <- c(0,1)
  y <- var.get.nc(nc, "temperature", c(NA,2), c(0,1),collapse=FALSE)
  tally <- testfun(x,y,tally)

  cat("Read numeric scalar ... ")
  x <- myint0
  y <- var.get.nc(nc, "int0")
  tally <- testfun(x,y,tally)

  cat("Read numeric empty array ... ")
  x <- numeric(0)
  dim(x) <- c(nstation,nempty)
  y <- var.get.nc(nc, "numempty")
  tally <- testfun(x,y,tally)

  cat("Read 2D char array ... ")
  x <- myname
  dim(x) <- length(x)
  y <- var.get.nc(nc, "name")
  tally <- testfun(x,y,tally)

  cat("Read 2D char slice ... ")
  x <- substring(myname[2:3],1,4)
  dim(x) <- length(x)
  y <- var.get.nc(nc, "name", c(1,2), c(4,2))
  tally <- testfun(x,y,tally)

  cat("Read 2D char slice as raw bytes ... ")
  x <- substring(myname[2:3],1,4)
  dim(x) <- length(x)
  x <- apply(x,MARGIN=1,FUN=charToRaw)
  y <- var.get.nc(nc, "name", c(1,2), c(4,2), rawchar=TRUE)
  tally <- testfun(x,y,tally)

  cat("Read 2D char slice as characters ... ")
  x <- myname[2:3]
  dim(x) <- length(x)
  y <- var.get.nc(nc, "name", c(1,2), c(NA,2))
  tally <- testfun(x,y,tally)

  cat("Read empty 2D char array ... ")
  x <- character(0)
  dim(x) <- 0
  y <- var.get.nc(nc, "name", NA, c(0,0),collapse=FALSE)
  tally <- testfun(x,y,tally)

  cat("Read 1D char slice ... ")
  x <- substring(myqcflag,2,3)
  y <- var.get.nc(nc, "qcflag", c(2), c(2))
  tally <- testfun(x,y,tally)

  cat("Read scalar char ... ")
  x <- mychar0
  y <- var.get.nc(nc, "char0")
  tally <- testfun(x,y,tally)

  if (format == "netcdf4") {
    cat("Read 1D string array ...")
    x <- myname
    dim(x) <- length(x)
    y <- var.get.nc(nc, "namestr")
    tally <- testfun(x,y,tally)

    cat("Read 1D string slice ...")
    x <- myname[2:3]
    dim(x) <- length(x)
    y <- var.get.nc(nc, "namestr", c(2), c(2))
    tally <- testfun(x,y,tally)

    cat("Read 1D int64 array as characters ...")
    x <- myid
    dim(x) <- length(x)
    y <- var.get.nc(nc, "stationid", fitnum=TRUE)
    tally <- testfun(x,y,tally)
  }

  cat("Read and unpack numeric array ... ")
  x <- mypackvar
  dim(x) <- length(x)
  y <- var.get.nc(nc, "packvar", unpack=TRUE)
  tally <- testfun(x,y,tally)

  cat("Check that closing any NetCDF handle closes the file for all handles ... ")
  close.nc(nc)
  y <- try(file.inq.nc(grpinfo$self), silent=TRUE)
  tally <- testfun(inherits(y, "try-error"), TRUE, tally)  

  cat("Check that garbage collector closes file that is not referenced ... ")
  attr(nc,"handle_ptr") <- NULL # NetCDF objects should not normally be modified
  rm(grpinfo)
  gc()
  y <- try(file.inq.nc(nc), silent=TRUE)
  tally <- testfun(inherits(y, "try-error"), TRUE, tally)
}

#-------------------------------------------------------------------------------#
#  UDUNITS calendar functions
#-------------------------------------------------------------------------------#

cat("utcal.nc - numeric values ...")
x <- matrix(data=c(1899, 1900, 1900, 1900, 1900, 1900,
                     12,    1,    1,    1,    1,    1,
		     31,    1,    1,    1,    1,    1,
		     23,    0,    1,    2,    3,    4,
		      0,    0,    0,    0,    0,    0,
		      0,    0,    0,    0,    0,    0),
	    ncol=6)
colnames(x) <- c("year","month","day","hour","minute","second")
y <- utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5))
tally <- testfun(x,y,tally)

cat("utcal.nc - string values ...")
x <- c("1899-12-31 23:00:00", "1900-01-01 00:00:00", "1900-01-01 01:00:00",
       "1900-01-01 02:00:00", "1900-01-01 03:00:00", "1900-01-01 04:00:00")
y <- utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="s")
tally <- testfun(x,y,tally)

cat("utcal.nc - POSIXct values ...")
x <- ISOdatetime(c(1899,1900,1900,1900,1900,1900),
                 c(  12,   1,   1,   1,   1,   1),
                 c(  31,   1,   1,   1,   1,   1),
                 c(  23,   0,   1,   2,   3,   4),
                 c(   0,   0,   0,   0,   0,   0),
                 c(   0,   0,   0,   0,   0,   0), tz="UTC")
y <- utcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(0:5), type="c")
tally <- testfun(x,y,tally)

cat("utinvcal.nc - numeric values ...")
x <- 6.416667
y <- utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", c(1900,1,1,5,25,0))
tally <- testfun(x,y,tally)

cat("utinvcal.nc - string values ...")
x <- 6.416667
y <- utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00", "1900-01-01 05:25:00")
tally <- testfun(x,y,tally)

cat("utinvcal.nc - POSIXct values ...")
x <- 6.416667
y <- utinvcal.nc("hours since 1900-01-01 00:00:00 +01:00",
         ISOdatetime(1900,1,1,5,25,0,tz="UTC"))
tally <- testfun(x,y,tally)

#-------------------------------------------------------------------------------#
#  Overall summary
#-------------------------------------------------------------------------------#
cat("Summary:", tally["pass"], "pass /", tally["fail"], "fail. ")

if (tally["fail"]==0) {
  cat("Package seems to work properly.\n")
} else {
  stop(tally["fail"]," of ",sum(tally)," test cases failed.")
}

#===============================================================================#

#===============================================================================#
#  SCRATCH
#===============================================================================#

