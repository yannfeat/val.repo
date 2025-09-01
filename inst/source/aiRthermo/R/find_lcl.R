find_lcl <-
function (Ptop, P0, T0, w0, deltaP) 
{
    if (is.na(Ptop) == TRUE) {
        stop("Ptop parameter must be different to NA")
    }
    else if (is.na(deltaP) == TRUE) {
        stop("deltaP parameter must be different to NA")
    }
    else if (is.na(P0) == TRUE) {
        stop("P0 parameter must be different to NA")
    }
    else if (is.na(T0) == TRUE) {
        stop("T0 parameter must be different to NA")
    }
    else if (is.na(w0) == TRUE) {
        stop("w0 parameter must be different to NA")
    }
    plcl = 0
    tlcl = 0
    wlcl = 0
    theta_lcl = 0
    gotIT = 0
    outvals <- .C("find_lcl_Rworld", as.double(Ptop), as.double(P0), 
        as.double(T0), as.double(w0), as.double(plcl), as.double(tlcl), 
        as.double(wlcl), as.double(theta_lcl), as.double(deltaP), 
        as.integer(gotIT))
    names(outvals) <- c("Ptop", "p", "t", "w", "plcl", "tlcl", 
        "wlcl", "theta_lcl", "deltaP", "gotIT")
    result <- list(outvals$plcl, outvals$tlcl, outvals$wlcl, 
        outvals$theta_lcl, outvals$gotIT)
    names(result) <- c("Plcl", "Tlcl", "wlcl", "thetalcl", "gotIt")
    return(result)
}
