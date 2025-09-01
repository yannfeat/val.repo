colormap <- function(
   dat,
   range=NULL,
   adjustment=NULL) {

   # Value range.
   if(length(range) < 2)
       range <- c(min(dat, na.rm=TRUE), max(dat, na.rm=TRUE))

   # Matrix of pivot colors.
   mtx <- matrix(NA, 0, 0)
   mtx.points <- double()
 
   # Single color or balanced rainbow.
   if(diff(range) == 0) {
       mtx <- matrix(NA, nrow=3, ncol=3)
       mtx[1,] <- c(100, 100, 100)/255
       mtx[2,] <- c(100, 100, 100)/255
       mtx[3,] <- c(100, 100, 100)/255
       mtx.points <- (0:2)/2
   } else {
       mtx <- matrix(NA, nrow=7, ncol=3)
       mtx[1,] <- c(255,  71, 189)/255
       mtx[2,] <- c(255,  65,  50)/255
       mtx[3,] <- c(255, 126,  25)/255
       mtx[4,] <- c(215, 210,   0)/255
       mtx[5,] <- c( 70, 240,  45)/255
       mtx[6,] <- c( 10, 190, 213)/255
       mtx[7,] <- c( 35, 130, 255)/255
       mtx.points <- (0:6)/6
   }

   # Brightness adjustment.
   if(length(adjustment) < 1) adjustment <- ""
   if(adjustment == "light") {
       mtx <- (1 - 0.97*(1 - mtx)^(1.5))
   }
   if(adjustment == "dark") {
       bright <- (0.3*(mtx[,1]) + 0.5*(mtx[,2]) + 0.1*(mtx[,3]))
       mtx[,1] <- (1 - 0.2*bright)*((mtx[,1])^(1.5))
       mtx[,2] <- (1 - 0.3*bright)*((mtx[,2])^(1.5))
       mtx[,3] <- (1 - 0.2*bright)*((mtx[,3])^(1.5))
   }

   # Make sure colors are usable.
   mtx <- apply(mtx, 2, function(x) {
       x <- pmax(x, 0.0, na.rm=TRUE)
       x <- pmin(x, 1.0, na.rm=TRUE)
       return(x) 
   })

   # Find usable data values.
   hues <- (dat - range[1])
   if(diff(range) != 0) hues <- hues/(range[2] - range[1])
   hues <- pmin(pmax(hues, 0.0), 1.0)

   # Interpolate color components. 
   mask <- which(is.finite(hues))
   comps <- matrix(0.5, nrow=length(dat), ncol=4)
   rownames(comps) <- seq(from=range[1], to=range[2], length.out=length(dat))
   colnames(comps) <- c("RED", "GREEN", "BLUE", "ALPHA")
   comps[mask,1] <- approx(x=mtx.points, y=mtx[,1], xout=hues[mask])$y
   comps[mask,2] <- approx(x=mtx.points, y=mtx[,2], xout=hues[mask])$y
   comps[mask,3] <- approx(x=mtx.points, y=mtx[,3], xout=hues[mask])$y
   comps[mask,4] <- 1.0

   # Create colors.
   output <- rgb(red=comps[,"RED"], green=comps[,"GREEN"],
       blue=comps[,"BLUE"], alpha=comps[,"ALPHA"])
   names(output) <- names(dat)
   attr(output, "components") <- comps
   return(output)
}
