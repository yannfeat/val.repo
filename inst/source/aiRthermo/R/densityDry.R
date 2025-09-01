densityDry <-
function (P, Temp, consts = export_constants()) 
{
    return(as.double(P/(Temp * consts["Rd"])))
}
