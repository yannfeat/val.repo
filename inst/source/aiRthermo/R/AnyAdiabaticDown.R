AnyAdiabaticDown <-
function (Pstart, Tstart, wstart, wcstart, Pend, deltaP) 
{
    tend <- 0
    wend <- 0
    wcend <- 0
    outvals <- .C("any_adiabatic_down_Rworld", as.double(Pstart), 
        as.double(Tstart), as.double(wstart), as.double(wcstart), 
        as.double(Pend), as.double(deltaP), as.double(tend), 
        as.double(wend), as.double(wcend))
    names(outvals) <- c("Pstart", "Tstart", "wstart", "wcstart", 
        "Pend", "deltaP", "tend", "wend", "wcend")
    outvals2 <- list(outvals$tend, outvals$wend, outvals$wcend)
    names(outvals2) <- c("Tend", "Wend", "Wcend")
    return(outvals2)
}
