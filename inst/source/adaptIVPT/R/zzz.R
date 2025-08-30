.onAttach<-function(libname, pkgname){   
   this.year <- format(Sys.time(), "%Y")
   packageStartupMessage("## Adaptive Designs for IVPT (", this.year, ")", sep="")
   packageStartupMessage("## This package reflects the views of the authors and should not be construed to represent the FDA's views or policies.")
}