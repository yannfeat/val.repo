adiabatic_ascent <-
function (Pstart, Tstart, wstart, Pend, deltaP = 1) 
{
    if (is.na(Pstart) == TRUE) {
        stop("Pstart parameter must be different to NA")
    }
    else if (is.na(Tstart) == TRUE) {
        stop("Tstart parameter must be different to NA")
    }
    else if (is.na(wstart) == TRUE) {
        stop("wstart parameter must be different to NA")
    }
    else if (is.na(Pend) == TRUE) {
        stop("Pend parameter must be different to NA")
    }
    else if (is.na(deltaP) == TRUE) {
        stop("deltaP parameter must be different to NA")
    }
    Tend = 0
    wend = 0
    outvals <- .C("adiabatic_ascent_Rworld", as.double(Pstart), 
        as.double(Tstart), as.double(wstart), as.double(Pend), 
        as.double(Tend), as.double(wend), as.double(deltaP))
    names(outvals) <- c("Pstart", "Tstart", "wstart", "Pend", 
        "Tend", "wend", "deltaP")
    result <- list(outvals$Tend, outvals$wend)
    names(result) <- c("Tend", "mixRatioEnd")
    return(result)
}
