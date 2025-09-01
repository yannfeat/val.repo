saturation_pressure_H2O <-
function (Temps) 
{
    Temps2 <- na.omit(Temps)
    nelems <- length(Temps2)
    outvals <- .C("saturation_pressure_H2O_Rworld", as.double(Temps2), 
        as.integer(nelems), result = double(nelems))
    Temps[!is.na(Temps)] <- outvals[["result"]]
    return(Temps)
}
