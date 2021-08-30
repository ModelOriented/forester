.onAttach <- function(libname, pkgname){
  options(mypkg.connection = stdin())
}
