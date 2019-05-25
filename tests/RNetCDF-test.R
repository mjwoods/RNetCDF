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
#  Copyright:  (C) 2010-2017 Pavel Michna, Milton Woods
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


#===============================================================================#
#  Load library
#===============================================================================#

library(RNetCDF)
has_bit64 <- require(bit64)

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
    str(x)
    print(attributes(x))
    cat("y:\n")
    str(y)
    print(attributes(y))
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

  if (format == "netcdf4") {
    ## Define a group
    ncroot <- nc
    nc <- grp.def.nc(nc, "testgrp")

    ## Define a type of each class:
    id_blob <- type.def.nc(nc, "blob", "opaque", size=128)
    inq_blob <- list(id=id_blob, name="blob", class="opaque", size=128)

    id_vector <- type.def.nc(nc, "vector", "vlen", basetype="NC_INT")
    inq_vector <- list(id=id_vector, name="vector", class="vlen",
                       size=NA, basetype="NC_INT")

    id_vector_char <- type.def.nc(nc, "vector_char", "vlen", basetype="NC_CHAR")
    inq_vector_char <- list(id=id_vector_char, name="vector_char", class="vlen",
                            size=NA, basetype="NC_CHAR")

    id_vector_blob <- type.def.nc(nc, "vector_blob", "vlen", basetype=id_blob)
    inq_vector_blob <- list(id=id_vector_blob, name="vector_blob", class="vlen",
                            size=NA, basetype="blob")

    id_factor <- type.def.nc(nc, "factor", "enum", basetype="NC_UBYTE",
                             names=c("peanut butter", "jelly"),
                             values=c(101, 102))
    inq_factor <- list(id=id_factor, name="factor", class="enum",
                       size=1, basetype="NC_UBYTE",
                       value=c("peanut butter"=101,"jelly"=102))

    id_struct <- type.def.nc(nc, "struct", "compound",
                             names=c("siteid", "height", "colour"),
                             subtypes=c("NC_INT", "NC_DOUBLE", "NC_SHORT"),
                             dimsizes=list(NULL, NULL, c(3)))
    inq_struct <- list(id=id_struct, name="struct", class="compound", size=18,
                       offset=c(siteid=0,height=4,colour=12),
                       subtype=c(siteid="NC_INT",height="NC_DOUBLE",colour="NC_SHORT"),
                       dimsizes=list("siteid"=NULL,"height"=NULL,"colour"=c(3)))

    typeids <- c(id_blob,id_vector,id_vector_char,id_vector_blob,id_factor,id_struct)
  }

  ##  Define variables
  var.def.nc(nc, "time", "NC_INT", "time")

  inq_temperature <- list()
  inq_temperature$id <- var.def.nc(nc, "temperature", "NC_DOUBLE", c(0,1),
                                   chunking=TRUE, chunksizes=c(5,1),
                                   deflate=5, shuffle=TRUE, big_endian=TRUE,
                                   fletcher32=TRUE)
  inq_temperature$name <- "temperature"
  inq_temperature$type <- "NC_DOUBLE"
  inq_temperature$ndims <- as.integer(2)
  inq_temperature$dimids <- as.integer(c(0,1))
  inq_temperature$natts <- as.integer(0)
  inq_temperature$chunksizes <- as.numeric(c(5,1))
  inq_temperature$deflate <- as.integer(5)
  inq_temperature$shuffle <- TRUE
  inq_temperature$big_endian <- TRUE
  inq_temperature$fletcher32 <- TRUE

  var.def.nc(nc, "packvar", "NC_BYTE", c("station"))
  var.def.nc(nc, "name", "NC_CHAR", c("max_string_length", "station"))
  var.def.nc(nc, "qcflag", "NC_CHAR", c("station"))
  var.def.nc(nc, "int0", "NC_INT", NA)
  var.def.nc(nc, "char0", "NC_CHAR", NA)
  var.def.nc(nc, "numempty", "NC_FLOAT", c("station","empty"))
  varcnt <- 8

  numtypes <- c("NC_BYTE", "NC_SHORT", "NC_INT", "NC_FLOAT", "NC_DOUBLE")

  if (format == "netcdf4") {
    var.def.nc(nc, "namestr", "NC_STRING", c("station"))
    var.def.nc(nc, "profile", id_vector, c("station","time"))
    var.def.nc(nc, "profile_char", id_vector_char, c("station","time"))
    var.def.nc(nc, "profile_blob", id_vector_blob, c("time"))
    var.def.nc(nc, "profile_scalar", id_vector, NA)
    var.def.nc(nc, "rawdata", id_blob, c("station","time"))
    var.def.nc(nc, "rawdata_scalar", id_blob, NA)
    var.def.nc(nc, "rawdata_vector", id_blob, c("station"))
    var.def.nc(nc, "snacks", "factor", c("station", "time"))
    var.def.nc(nc, "person", "struct", c("station", "time"))
    varcnt <- varcnt+10

    numtypes <- c(numtypes, "NC_UBYTE", "NC_USHORT", "NC_UINT")

    if (has_bit64) {
      var.def.nc(nc, "stationid", "NC_UINT64", c("station"))
      varcnt <- varcnt+1
      numtypes <- c(numtypes, "NC_INT64", "NC_UINT64")
    }
  }

  for (numtype in numtypes) {
    var.def.nc(nc, numtype, numtype, c("station"))
    var.def.nc(nc, paste(numtype,"_int",sep=""), numtype, c("station"))

    var.def.nc(nc, paste(numtype,"_fill",sep=""), numtype, c("station"))
    att.put.nc(nc, paste(numtype,"_fill",sep=""), "_FillValue", numtype, 99)
    var.def.nc(nc, paste(numtype,"_intfill",sep=""), numtype, c("station"))
    att.put.nc(nc, paste(numtype,"_intfill",sep=""), "_FillValue", numtype, 99)

    var.def.nc(nc, paste(numtype,"_pack",sep=""), numtype, c("station"))
    att.put.nc(nc, paste(numtype,"_pack",sep=""), "scale_factor", numtype, 10)
    att.put.nc(nc, paste(numtype,"_pack",sep=""), "add_offset", numtype, 5)
    var.def.nc(nc, paste(numtype,"_intpack",sep=""), numtype, c("station"))
    att.put.nc(nc, paste(numtype,"_intpack",sep=""), "scale_factor", numtype, 10)
    att.put.nc(nc, paste(numtype,"_intpack",sep=""), "add_offset", numtype, 5)

    varcnt <- varcnt+6
  }

  ##  Set a _FillValue attribute for temperature
  att.put.nc(nc, "temperature", "_FillValue", "NC_DOUBLE", -99999.9)
  inq_temperature$natts <- inq_temperature$natts + 1

  ## Define the packing used by packvar
  id_double <- type.inq.nc(nc, "NC_DOUBLE")$id
  att.put.nc(nc, "packvar", "scale_factor", id_double, 10)
  att.put.nc(nc, "packvar", "add_offset", "NC_DOUBLE", -5)

  ## Define some additional test attributes:
  att_text <- "This is some text"
  att_text2 <- c("This is string 1", "This is string 2")
  att.put.nc(nc, "NC_GLOBAL", "char_att", "NC_CHAR", att_text)
  att.put.nc(nc, "name", "char_att", "NC_CHAR", att_text)
  att.put.nc(nc, "name", "raw_att", "NC_CHAR", charToRaw(att_text))
  if (format == "netcdf4") {
    att.put.nc(nc, "temperature", "string_att", "NC_STRING", att_text2)
    inq_temperature$natts <- inq_temperature$natts + 1
    if (has_bit64) {
      hugeint <- as.integer64("-1234567890123456789")
      att.put.nc(nc, "temperature", "int64_att", "NC_INT64", hugeint)
      inq_temperature$natts <- inq_temperature$natts + 1
    }
  }

  ##  Define variable values
  mytime        <- c(1:2)
  mytemperature <- matrix(c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, NA, NA, 9.9),ncol=ntime)
  mypackvar     <- seq_len(5)*10-5
  myname        <- c("alfa", "bravo", "charlie", "delta", "echo")
  myqcflag      <- "ABCDE"
  myint0        <- 12345
  mychar0       <- "?"

  mysmall       <- as.double(c(1,2,3,4,5))
  mybig         <- mysmall*1e100
  myminus       <- -mysmall
  mysmallfill   <- as.double(c(1,2,NA,4,5))
  mybigfill     <- mysmallfill*1e100
  mypack        <- mysmall*10+5

  if (format == "netcdf4") {
    profiles      <- vector("list", nstation*ntime)
    dim(profiles) <- c(nstation, ntime)
    for (ii in seq_len(nstation)) {
      for (jj in seq_len(ntime)) {
	profiles[[ii,jj]] <- seq_len(ii+jj)*(ii+jj)
      }
    }

    profiles_char <- lapply(profiles,function(x) {paste(as.character(x),collapse=",")})
    dim(profiles_char) <- dim(profiles)

    rawdata <- as.raw(seq_len(nstation*ntime*128) %% 256)
    dim(rawdata) <- c(128,nstation,ntime)

    profiles_blob <- list(rawdata[,1:2,1], rawdata[,3:5,1])
    dim(profiles_blob) <- ntime

    snack_foods <- names(inq_factor$value)
    snacks <- factor(rep(snack_foods,times=5),
                         levels=snack_foods)
    dim(snacks) <- c(nstation, ntime)

    person <- list(siteid=array(rep(seq(1,nstation),ntime), c(nstation,ntime)),
                   height=array(1+0.1*seq(1,nstation*ntime), c(nstation,ntime)),
                   colour=array(rep(c(0,0,0,64,128,192),nstation), c(3,nstation,ntime)))
  }

  ## Define some user-defined test attributes:
  if (format == "netcdf4") {
    person1 <- list(siteid=array(person$siteid[1,1], 1),
                    height=array(person$height[1,1], 1),
                    colour=array(person$colour[,1,1], c(3,1)))
    person3 <- list(siteid=array(person$siteid[1:3,1], 3),
                    height=array(person$height[1:3,1], 3),
                    colour=array(person$colour[,1:3,1], c(3,3)))
    att.put.nc(nc, "NC_GLOBAL", "compound_scal_att", "struct", person1)
    att.put.nc(nc, "NC_GLOBAL", "compound_vect_att", "struct", person3)
    att.put.nc(nc, "NC_GLOBAL", "enum_scal_att", "factor", snacks[1])
    att.put.nc(nc, "NC_GLOBAL", "enum_vect_att", "factor", snacks[1:3])
    att.put.nc(nc, "NC_GLOBAL", "opaque_scal_att", "blob", rawdata[,1,1])
    att.put.nc(nc, "NC_GLOBAL", "opaque_vect_att", "blob", rawdata[,1,])
    att.put.nc(nc, "NC_GLOBAL", "vector_scal_att", "vector", profiles[1])
    att.put.nc(nc, "NC_GLOBAL", "vector_vect_att", "vector", profiles[1:3])
  }

  ##  Put the data
  cat("Writing variables ...\n")
  var.put.nc(nc, "time", mytime, 1, length(mytime))
  var.put.nc(nc, "temperature", mytemperature, c(1,1), c(nstation,ntime),
             cache_preemption=0.5)
  var.put.nc(nc, "packvar", mypackvar, pack=TRUE)
  var.put.nc(nc, "name", myname, c(1,1), c(nstring,nstation))
  var.put.nc(nc, "qcflag", charToRaw(myqcflag))
  var.put.nc(nc, "int0", myint0)
  var.put.nc(nc, "char0", mychar0)

  if (format == "netcdf4") {
    var.put.nc(nc, "namestr", myname)
    var.put.nc(nc, "profile", profiles)
    var.put.nc(nc, "profile_char", profiles_char)
    var.put.nc(nc, "profile_blob", profiles_blob)
    var.put.nc(nc, "profile_scalar", profiles[1])
    var.put.nc(nc, "rawdata", rawdata)
    var.put.nc(nc, "rawdata_scalar", rawdata[,1,1])
    var.put.nc(nc, "rawdata_vector", rawdata[,,1])
    var.put.nc(nc, "snacks", snacks)
    var.put.nc(nc, "person", person)
    if (has_bit64) {
      myid <- as.integer64("1234567890123456789")+c(0,1,2,3,4)
      var.put.nc(nc, "stationid", myid)
    }
  }

  for (numtype in numtypes) {
    # Should not succeed except for NC_DOUBLE:
    y <- try(var.put.nc(nc, numtype, mybig), silent=TRUE)
    tally <- testfun(inherits(y, "try-error"), numtype!="NC_DOUBLE", tally)

    y <- try(var.put.nc(nc, paste(numtype,"_fill",sep=""), mybigfill), silent=TRUE)
    tally <- testfun(inherits(y, "try-error"), numtype!="NC_DOUBLE", tally)

    # Should not succeed for unsigned types:
    y <- try(var.put.nc(nc, numtype, myminus), silent=TRUE)
    tally <- testfun(inherits(y, "try-error"),
                     any(numtype==c("NC_UBYTE", "NC_USHORT", "NC_UINT", "NC_UINT64")),
                     tally) 

    # Should succeed for all types:
    var.put.nc(nc, numtype, mysmall)
    var.put.nc(nc, paste(numtype,"_int",sep=""), as.integer(mysmall))
    var.put.nc(nc, paste(numtype,"_fill",sep=""), mysmallfill)
    var.put.nc(nc, paste(numtype,"_intfill",sep=""), as.integer(mysmallfill))
    var.put.nc(nc, paste(numtype,"_pack",sep=""), mypack, pack=TRUE)
    var.put.nc(nc, paste(numtype,"_intpack",sep=""), as.integer(mypack), pack=TRUE)
  }

  if (format == "netcdf4") {
    # Check chunk cache settings for temperature:
    cat("Check chunk cache settings after writing temperature ...")
    x <- var.inq.nc(nc, "temperature")$cache_preemption
    if (is.na(x)) {
      cat("Feature not available in this NetCDF library version.\n")
    } else {
      y <- 0.5
    }
    tally <- testfun(x,y,tally)
  }

#  sync.nc(nc)
  if (format == "netcdf4") {
    close.nc(ncroot)
    ncroot <- open.nc(ncfile)
    nc <- grp.inq.nc(ncroot, "testgrp")$self
  } else {
    close.nc(nc)
    nc <- open.nc(ncfile)
  }

  cat("Check file format ...")
  x <- file.inq.nc(nc)$format
  y <- format
  tally <- testfun(x,y,tally)

  ## Display file structure
  print.nc(nc)

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

    if (has_bit64) {
      cat("Read NC_INT64 variable attribute ...")
      x <- hugeint
      y <- att.get.nc(nc, "temperature", "int64_att", fitnum=TRUE)
      tally <- testfun(x,y,tally)

      cat("Read NC_INT64 variable attribute as numeric ...")
      x <- suppressWarnings(as.numeric(hugeint))
      y <- att.get.nc(nc, "temperature", "int64_att")
      tally <- testfun(x,y,tally)
    }
  }

  grpinfo <- grp.inq.nc(nc)
  cat("Inquire about groups in file/group ...")
  tally <- testfun(grpinfo$grps,list(),tally)
  cat("Inquire about dimension ids in file/group ...")
  tally <- testfun(grpinfo$dimids,c(0:3),tally)
  cat("Inquire about variable ids in file/group ...")
  tally <- testfun(grpinfo$varids,c(0:(varcnt-1)),tally)
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
  if (format == "netcdf4") {
    cat("Inquire about user-defined types in file/group ...")
    tally <- testfun(grpinfo$typeids,typeids,tally)
  }

  cat("Read integer vector as double ... ")
  x <- mytime
  dim(x) <- length(x)
  y <- var.get.nc(nc, 0)
  tally <- testfun(x,y,tally)
  tally <- testfun(is.double(y),TRUE,tally)

  for (numtype in numtypes) {
    cat("Read", numtype, "...")
    x <- mysmall
    dim(x) <- length(x)
    y <- var.get.nc(nc, numtype)
    tally <- testfun(x,y,tally)
    tally <- testfun(is.double(y),TRUE,tally)
    y <- var.get.nc(nc, paste(numtype,"_int",sep=""))
    tally <- testfun(x,y,tally)
    tally <- testfun(is.double(y),TRUE,tally)

    x <- mysmallfill
    dim(x) <- length(x)
    y <- var.get.nc(nc, paste(numtype,"_fill",sep=""))
    tally <- testfun(x,y,tally)
    tally <- testfun(is.double(y),TRUE,tally)
    y <- var.get.nc(nc, paste(numtype,"_intfill",sep=""))
    tally <- testfun(x,y,tally)
    tally <- testfun(is.double(y),TRUE,tally)

    x <- mypack
    dim(x) <- length(x)
    y <- var.get.nc(nc, paste(numtype,"_pack",sep=""), unpack=TRUE)
    tally <- testfun(x,y,tally)
    tally <- testfun(is.double(y),TRUE,tally)
    y <- var.get.nc(nc, paste(numtype,"_intpack",sep=""), unpack=TRUE)
    tally <- testfun(x,y,tally)
    tally <- testfun(is.double(y),TRUE,tally)
  }

  cat("Read integer vector as smallest R type ... ")
  x <- mytime
  dim(x) <- length(x)
  y <- var.get.nc(nc, 0, fitnum=TRUE)
  tally <- testfun(x,y,tally)
  tally <- testfun(is.integer(y),TRUE,tally)

  for (numtype in numtypes) {
    x <- mysmall
    if (has_bit64 && any(numtype==c("NC_INT64","NC_UINT64"))) {
      x <- as.integer64(x)
    }
    dim(x) <- length(x)
    y <- var.get.nc(nc, numtype, fitnum=TRUE)
    tally <- testfun(x,y,tally)
    tally <- testfun(is.integer(y),
                     any(numtype==c("NC_BYTE","NC_UBYTE","NC_SHORT","NC_USHORT","NC_INT")),
                     tally)

    x <- mysmallfill
    if (has_bit64 && any(numtype==c("NC_INT64","NC_UINT64"))) {
      x <- as.integer64(x)
    }
    dim(x) <- length(x)
    y <- var.get.nc(nc, paste(numtype,"_fill",sep=""), fitnum=TRUE)
    tally <- testfun(x,y,tally)
    tally <- testfun(is.integer(y),
                     any(numtype==c("NC_BYTE","NC_UBYTE","NC_SHORT","NC_USHORT","NC_INT")),
                     tally)
  }

  cat("Read numeric matrix ... ")
  x <- mytemperature
  y <- var.get.nc(nc, "temperature", cache_preemption=0.4)
  tally <- testfun(x,y,tally)

  cat("Inquire about numeric variable ...")
  x <- inq_temperature
  y <- var.inq.nc(nc, "temperature")
  var_inq_names <- c("id", "name", "type", "ndims", "dimids", "natts")
  if (format == "netcdf4") {
    var_inq_names_nc4 <- c(var_inq_names, "chunksizes", "deflate", "shuffle",
                           "big_endian", "fletcher32")
    tally <- testfun(x[var_inq_names_nc4], y[var_inq_names_nc4], tally)
    preempt <- y$cache_preemption
    if (!is.na(preempt)) {
      tally <- testfun(0.4, preempt, tally)
    }
  } else {
    tally <- testfun(x[var_inq_names], y[var_inq_names], tally)
  }

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

    if (has_bit64) {
      cat("Read 1D int64 array as integer64 ...")
      x <- myid
      dim(x) <- length(x)
      y <- var.get.nc(nc, "stationid", fitnum=TRUE)
      tally <- testfun(x,y,tally)
    }

    cat("Read details of user-defined types ...")
    x <- inq_blob
    y <- type.inq.nc(nc, id_blob)
    tally <- testfun(x,y,tally)

    # Reported size may depend on netcdf version and pointer size:
    x <- inq_vector[-4]
    y <- type.inq.nc(nc, id_vector)[-4]
    tally <- testfun(x,y,tally)

    x <- inq_vector_char[-4]
    y <- type.inq.nc(nc, id_vector_char)[-4]
    tally <- testfun(x,y,tally)

    x <- inq_vector_blob[-4]
    y <- type.inq.nc(nc, id_vector_blob)[-4]
    tally <- testfun(x,y,tally)

    x <- inq_factor
    y <- type.inq.nc(nc, id_factor)
    tally <- testfun(x,y,tally)

    x <- inq_factor[1:5]
    y <- type.inq.nc(nc, id_factor, fields=FALSE)
    tally <- testfun(x,y,tally)

    # Size and offset of compound types may differ between writing and reading.
    # The layout for writing (reading) is defined by the user (compiler).
    x <- inq_struct[c(-4,-5)]
    y <- type.inq.nc(nc, id_struct)[c(-4,-5)]
    tally <- testfun(x,y,tally)

    x <- inq_struct[1:3]
    y <- type.inq.nc(nc, id_struct, fields=FALSE)[-4]
    tally <- testfun(x,y,tally)

    cat("Read vlen as double ...")
    x <- profiles
    y <- var.get.nc(nc, "profile")
    tally <- testfun(x,y,tally)
    tally <- testfun(isTRUE(all(sapply(y,is.double))), TRUE, tally)

    cat("Read vlen as integer ...")
    x <- profiles
    y <- var.get.nc(nc, "profile", fitnum=TRUE)
    tally <- testfun(x,y,tally)
    tally <- testfun(isTRUE(all(sapply(y,is.integer))), TRUE, tally)

    cat("Read vlen scalar ...")
    x <- profiles[1]
    y <- var.get.nc(nc, "profile_scalar")
    tally <- testfun(x,y,tally)

    cat("Read vlen as character ...")
    x <- profiles_char
    y <- var.get.nc(nc, "profile_char")
    tally <- testfun(x,y,tally)

    cat("Read vlen as raw ...")
    x <- lapply(profiles_char,charToRaw)
    dim(x) <- dim(profiles_char)
    y <- var.get.nc(nc, "profile_char", rawchar=TRUE)
    tally <- testfun(x,y,tally)

    cat("Read opaque ...")
    x <- rawdata
    y <- var.get.nc(nc, "rawdata")
    tally <- testfun(x,y,tally)

    cat("Read opaque scalar ...")
    x <- rawdata[,1,1]
    dim(x) <- length(x)
    y <- var.get.nc(nc, "rawdata_scalar")
    tally <- testfun(x,y,tally)

    cat("Read opaque vector ...")
    x <- rawdata[,,1]
    y <- var.get.nc(nc, "rawdata_vector")
    tally <- testfun(x,y,tally)

    cat("Read opaque vlen ...")
    x <- profiles_blob
    y <- var.get.nc(nc, "profile_blob")
    tally <- testfun(x,y,tally)

    cat("Read enum ...")
    x <- snacks
    y <- var.get.nc(nc, "snacks")
    tally <- testfun(x,y,tally)

    cat("Read compound ...")
    x <- person
    y <- var.get.nc(nc, "person")
    tally <- testfun(x,y,tally)

    cat("Read compound scalar attribute ...")
    x <- person1
    y <- att.get.nc(nc, "NC_GLOBAL", "compound_scal_att")
    tally <- testfun(x,y,tally)

    cat("Read compound vector attribute ...")
    x <- person3
    y <- att.get.nc(nc, "NC_GLOBAL", "compound_vect_att")
    tally <- testfun(x,y,tally)

    cat("Read enum scalar attribute ...")
    x <- snacks[1]
    y <- att.get.nc(nc, "NC_GLOBAL", "enum_scal_att")
    tally <- testfun(x,y,tally)

    cat("Read enum vector attribute ...")
    x <- snacks[1:3]
    y <- att.get.nc(nc, "NC_GLOBAL", "enum_vect_att")
    tally <- testfun(x,y,tally)

    cat("Read opaque scalar attribute ...")
    x <- rawdata[,1,1]
    dim(x) <- c(length(x),1)
    y <- att.get.nc(nc, "NC_GLOBAL", "opaque_scal_att")
    tally <- testfun(x,y,tally)

    cat("Read opaque vector attribute ...")
    x <- rawdata[,1,]
    y <- att.get.nc(nc, "NC_GLOBAL", "opaque_vect_att")
    tally <- testfun(x,y,tally)

    cat("Read vlen scalar attribute ...")
    x <- profiles[1]
    y <- att.get.nc(nc, "NC_GLOBAL", "vector_scal_att")
    tally <- testfun(x,y,tally)

    cat("Read vlen vector attribute ...")
    x <- profiles[1:3]
    y <- att.get.nc(nc, "NC_GLOBAL", "vector_vect_att")
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

# Test if udunits support is available:
if (inherits(try(utcal.nc("seconds since 1970-01-01", 0)), "try-error")) {
  warning("UDUNITS calendar conversions not supported by this build of RNetCDF")

} else {

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

}

# Check that package can be unloaded:
cat("Unload RNetCDF ...")
detach("package:RNetCDF",unload=TRUE)

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

