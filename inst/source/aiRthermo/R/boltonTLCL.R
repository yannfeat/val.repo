boltonTLCL <-
function (TempCelsius, rh, consts = export_constants()) 
{
    return(55 + 1/(1/(TempCelsius - 55) - log(rh/100)/2840))
}
