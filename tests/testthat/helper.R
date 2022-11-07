# Initialize and finalize pbdMPI once per session:
with_pbdMPI <- FALSE
if (requireNamespace("pbdMPI")) {
  try({
    if (!pbdMPI::comm.is.null() == -1) {
      pbdMPI::init()
      withr::defer(
        if (!pbdMPI::is.finalized()) {
          #print("Finalizing MPI")
          pbdMPI::finalize(mpi.finalize=TRUE)
        }, envir = .GlobalEnv)
      with_pbdMPI <- TRUE
    }
  })
}

# Function to skip if pbdMPI is not available or working:
skip_not_pbdMPI <- function() {
  skip_if_not_installed("pbdMPI")
  if (!exists("with_pbdMPI") || !isTRUE(with_pbdMPI)) {
      skip("pbdMPI not initialized by setup.R")
  }
}

# Function to test features that could be unsupported.
# If an error occurs:
# - Skip remaining tests if error message matches regex
# - Otherwise signal the error
# Otherwise return results as usual
skip_unsupported <- function(expr, regex="not support|not turned on") {
  result <- tryCatch(expr, error=function(e) e)
  if (inherits(result, "error")) {
    message <- conditionMessage(result)
    if (grepl(regex, message)) {
      skip(message)
    } else {
      signalCondition(result)
    }
  }
  return(result)
}

