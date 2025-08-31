welcome<- function(){
msg <- c(paste0(
" Welcome to
    _____________     ____________     _____________
   |             |   |            |   |             |
   |    _____    |   |    ________|   |    _____    |
   |   |     |   |   |   |            |   |     |   |
   |   |_____|   |   |   |________    |   |_____|   |
   |             |   |            |   |             |
   |    _____    |   |    ________|   |    _________|
   |   |     |   |   |   |            |   |
   |   |     |   |   |   |________    |   |
   |   |     |   |   |            |   |   |
   |___|     |___|   |____________|   |___|            version ",

packageVersion("AEP")),"\nType 'citation(\"AEP\")' for citing this R package in publications.")
 return(msg)
}
.onAttach <- function(libname, pkgname) {
  mess <- welcome()
  if(!interactive())
    mess[1] <- paste("Package 'AEP' version", packageVersion("AEP"))
    packageStartupMessage(mess)
  invisible()
  }
