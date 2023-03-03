.onAttach<-function(libname, pkgname){
  requireNamespace("utils")
  cit<-citation(pkgname)
  txt<-paste(c(format(cit,"citation")),collapse="\n\n")
  suppressWarnings({packageStartupMessage(txt)})
}
