.onAttach <- function(libname, pkgname) {
  pd <- utils::packageDescription(pkgname);
  packageStartupMessage(pkgname, " v", pd$Version, " successfully loaded. See Quick Start Guide for help getting started:\n https://github.com/prodriguezsosa/conText/blob/master/vignettes/quickstart.md");
}
