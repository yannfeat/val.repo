rh2w <-
function (P, Temp, rh, consts = export_constants()) 
{
    return(saturation_mixing_ratio(P, Temp, consts) * rh/100)
}
