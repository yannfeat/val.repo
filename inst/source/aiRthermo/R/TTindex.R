TTindex <-
function (Ps, Ts, ws, doLog = 0) 
{
    if ((length(Ps) != length(Ts)) | (length(Ps) != length(ws)) | 
        (length(Ts) != length(ws))) {
        stop("Input parameters ARE wrong, Ps, Ts, ws must be the same length")
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
    outvals <- .C("TTindex_Rworld", as.double(Ps2), as.double(Ts2), 
        as.double(ws2), as.integer(nlevs), as.integer(doLog), 
        result = double(1))
    if (outvals[["result"]] == -99999999) {
        warning("Needed levels not found")
        return(NA)
    }
    else {
        return(outvals[["result"]])
    }
}
