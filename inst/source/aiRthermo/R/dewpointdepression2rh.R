dewpointdepression2rh <-
function (P, Temp, dpd, consts = export_constants()) 
{
    Td = Temp - dpd
    return(100 * saturation_mixing_ratio(P, Td, consts)/saturation_mixing_ratio(P, 
        Temp, consts))
}
