.onAttach <- function(libname, pkgname) {
  pd <- utils::packageDescription(pkgname);
  packageStartupMessage(pkgname, " v", pd$Version, " successfully loaded. Note this version of the package comes with significant changes to the conText() function.
                        See Quick Start Guide for help getting started and instructions for accessing older versions of the package:\n https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md");
}
