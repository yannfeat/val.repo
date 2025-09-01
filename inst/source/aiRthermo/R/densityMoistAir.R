densityMoistAir <-
function (P, Temp, w, consts = export_constants()) 
{
    Tv = virtual_temperature(P, Temp, w, consts)
    return(as.double(P/(Tv * consts["Rd"])))
}
