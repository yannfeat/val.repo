CAPE_CIN <-
function (Ps, Ts, ws, deltaP = 5, P0 = NA, T0 = NA, w0 = NA, 
    PlowTop = NA, precoolType = "none", doLog = 0, getLiftedBack = FALSE, 
    upToTop = TRUE, checkBuoyancy = 0) 
{
    if ((length(Ps) != length(Ts)) | (length(Ps) != length(ws)) | 
        (length(Ts) != length(ws))) {
        stop("Input parameters ARE wrong, Ps, Ts, ws must be the same length")
    }
    if (is.na(deltaP) == TRUE) {
        stop("deltaP parameter must be different to NA")
    }
    N_Ps <- which(is.na(Ps) == TRUE)
    N_Ts <- which(is.na(Ts) == TRUE)
    N_ws <- which(is.na(ws) == TRUE)
    Ps[c(N_Ts, N_ws)] <- NA
    Ts[c(N_Ps, N_ws)] <- NA
    ws[c(N_Ts, N_Ps)] <- NA
    Ps2 <- na.omit(Ps)
    Ts2 <- na.omit(Ts)
    ws2 <- na.omit(ws)
    nlevs <- length(Ts2)
    if (Ps2[1] > Ps2[2]) {
        Nmin = 1
    }
    else {
        Nmin = nlevs
    }
    if (is.na(P0) | is.na(T0) | is.na(w0)) {
        if (is.na(PlowTop)) {
            P0 = Ps2[Nmin]
            T0 = Ts2[Nmin]
            w0 = ws2[Nmin]
            usePTW0 <- 1
        }
        else {
            P0 = Ps2[Nmin]
            T0 = Ts2[Nmin]
            w0 = ws2[Nmin]
            usePTW0 <- 0
        }
    }
    else {
        if (is.na(PlowTop)) {
            usePTW0 <- 1
        }
        else {
            usePTW0 <- 0
        }
    }
    if (is.na(PlowTop)) {
        plow = 0
    }
    else {
        plow = PlowTop
    }
    precool <- 0
    if (precoolType == "adiabatic") {
        precool <- 1
    }
    if (precoolType == "isobaric") {
        precool <- 2
    }
    if (abs(plow) > 0) {
        precool <- 2
    }
    if (getLiftedBack == TRUE) {
        Nlifted = ((Ps2[1] - Ps2[nlevs])/deltaP) + 10
        Tl = rep(0, Nlifted)
        wl = rep(0, Nlifted)
        Pl = rep(0, Nlifted)
    }
    else {
        Nlifted = 0
        Tl = export_constants()[["MISSING_VALUE"]]
        wl = export_constants()[["MISSING_VALUE"]]
        Pl = export_constants()[["MISSING_VALUE"]]
    }
    if (is.na(upToTop)) {
        uptotop = 0
    }
    else {
        if (upToTop == TRUE) {
            uptotop = 1
        }
        else {
            uptotop = 0
        }
    }
    airStart <- rep(0, 6)
    apLCL <- rep(0, 6)
    apLFC <- rep(0, 6)
    apEL <- rep(0, 6)
    cape = 0
    cin = 0
    gotLCL <- as.integer(0)
    gotLFC <- as.integer(0)
    gotEL <- as.integer(0)
    result <- as.integer(0)
    Olifted <- as.integer(0)
    outvals <- .C("CAPE_CIN_Rworld", as.double(P0), as.double(T0), 
        as.double(w0), as.integer(usePTW0), as.double(plow), 
        as.integer(precool), as.double(airStart), as.double(Ps2), 
        as.double(Ts2), as.double(ws2), as.integer(nlevs), as.double(cape), 
        as.double(cin), as.double(apLCL), as.double(apLFC), as.double(apEL), 
        as.integer(doLog), as.double(deltaP), as.integer(gotLCL), 
        as.integer(gotLFC), as.integer(gotEL), as.double(Pl), 
        as.double(Tl), as.double(wl), as.integer(Nlifted), as.integer(Olifted), 
        as.integer(result), as.integer(uptotop), as.integer(checkBuoyancy))
    names(outvals) <- c("P0", "T0", "w0", "usePTW0", "PlowTop", 
        "precool", "airStart", "Ps", "Ts", "ws", "nlevs", "cape", 
        "cin", "apLCL", "apLFC", "apEL", "doLog", "deltaP", "gotLCL", 
        "gotLFC", "gotEL", "Pl", "Tl", "wl", "Nlifted", "Olifted", 
        "result", "upToTop", "checkBouyancy")
    if (outvals$Olifted > 1) {
        outvals$Pl = outvals$Pl[1:outvals$Olifted]
        outvals$Tl = outvals$Tl[1:outvals$Olifted]
        outvals$wl = outvals$wl[1:outvals$Olifted]
    }
    if (outvals$result == 1) {
        warning("Your initial conditions are not reasonable. You are outside the sounding.")
    }
    else if (outvals$result == 2 | outvals$result == 3 | outvals$result == 
        4 | outvals$result == 5 | outvals$result == 6 | outvals$result == 
        7 | outvals$result == 11 | outvals$result == 12 | outvals$result == 
        13 | outvals$result == 14) {
        warning(paste("Code:", outvals$result, ": Ambient parcel at initial state not inside sounding.", 
            sep = " "))
    }
    else if (outvals$result == 8) {
        warning("LFC and EL not found after arriving to the top of the sounding.")
    }
    else if (outvals$result == 9) {
        warning("You must not be here. Send us an email telling You have found a Tyrannosaurus Rex.")
    }
    else if (outvals$result == 10) {
        warning("You must not be here. Send us an email telling You have found a Velociraptor.")
    }
    else if (outvals$result == 16) {
        warning("The parcel was never boyant. CIN is underestimated.")
    }
    if (is.nan(outvals$cape)) {
        outvals$cape = NA
    }
    if (is.nan(outvals$cin)) {
        outvals$cin = NA
    }
    if (!is.na(outvals$cape) & (outvals$cape <= -100000)) {
        outvals$cape = NA
    }
    if (!is.na(outvals$cin) & (outvals$cin <= -100000)) {
        outvals$cin = NA
    }
    if (outvals$gotLCL == 0 & outvals$gotLFC == 0 & outvals$gotEL == 
        0) {
        print("Impossible to calculate the CAPE and the CIN")
    }
    else if (outvals$gotLCL == 1 & outvals$gotLFC == 0 & outvals$gotEL == 
        0) {
        print("Impossible to calculate the CAPE and the CIN.")
    }
    else if (outvals$gotLCL == 1 & outvals$gotLFC == 1 & outvals$gotEL == 
        0) {
        print("The CIN was calculated. Not the CAPE.")
    }
    else if (outvals$gotLCL == 1 & outvals$gotLFC == 1 & outvals$gotEL == 
        1) {
        print("The CIN and the CAPE were calculated.")
    }
    outvals2 <- list(outvals$airStart, outvals$cape, outvals$cin, 
        outvals$apLCL, outvals$apLFC, outvals$apEL, outvals$gotLCL, 
        outvals$gotLFC, outvals$gotEL, outvals$Pl, outvals$Tl, 
        outvals$wl, outvals$Olifted, outvals$upToTop, outvals$result)
    names(outvals2) <- c("airStart", "cape", "cin", "apLCL", 
        "apLFC", "apEL", "gotLCL", "gotLFC", "gotEL", "Pl", "Tl", 
        "wl", "Olifted", "upToTop", "outCode")
    outvals2$apLCL <- as.list(outvals2$apLCL)
    names(outvals2$apLCL) <- c("P", "Temp", "w", "theta", "virtualT", 
        "wsat")
    outvals2$apLFC <- as.list(outvals2$apLFC)
    names(outvals2$apLFC) <- c("P", "Temp", "w", "theta", "virtualT", 
        "wsat")
    outvals2$apEL <- as.list(outvals2$apEL)
    names(outvals2$apEL) <- c("P", "Temp", "w", "theta", "virtualT", 
        "wsat")
    return(outvals2)
}
