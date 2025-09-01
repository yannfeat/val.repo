Sindex <-
function (Ps, Ts, ws, deltaP, doLog = 0) 
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
    outvals <- .C("Sindex_Rworld", as.double(Ps2), as.double(Ts2), 
        as.double(ws2), as.integer(nlevs), as.integer(doLog), 
        as.double(deltaP), result = double(1))
    if (outvals[["result"]] == -99999999) {
        warning("Needed levels (850, 700 and 500 hPa) not found")
        return(NA)
    }
    else {
        return(outvals[["result"]])
    }
}
