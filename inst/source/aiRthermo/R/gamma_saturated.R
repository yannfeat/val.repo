gamma_saturated <-
function (Ps, Temps) 
{
    if (length(Ps) != length(Temps)) {
        stop("Input parameters ARE wrong, Ps, Temps must be the same length")
    }
    N_Ps <- which(is.na(Ps) == TRUE)
    N_Temps <- which(is.na(Temps) == TRUE)
    Ps[N_Temps] <- NA
    Temps[N_Ps] <- NA
    Temps2 <- na.omit(Temps)
    Ps2 <- na.omit(Ps)
    nelems <- length(Temps2)
    outvals <- .C("gamma_sat_P_Rworld", as.double(Ps2), as.double(Temps2), 
        as.integer(nelems), result = double(nelems))
    outvals[["result"]]
    Temps[!is.na(Temps)] <- outvals[["result"]]
    return(Temps)
}
