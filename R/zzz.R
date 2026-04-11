.onAttach <- function(libname, pkgname) {
  pd <- utils::packageDescription(pkgname);
  packageStartupMessage(pkgname, " v", pd$Version, " successfully loaded. Note this version of the package comes with significant changes to the conText() function.
                        See Quick Start Guide for help getting started and instructions for accessing older versions of the package:\n https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md");
}

.onLoad <- function(libname, pkgname) {
  if (identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "TRUE")) {
    Sys.setenv(
      "OMP_THREAD_LIMIT" = 2,
      "OMP_NUM_THREADS" = 1,
      "OPENBLAS_NUM_THREADS" = 1,
      "MKL_NUM_THREADS" = 1,
      "VECLIB_MAXIMUM_THREADS" = 1,
      "RCPP_PARALLEL_NUM_THREADS" = 1
    )
  }
}
