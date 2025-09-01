TTdP2rh <-
function (Temp, Td, P, consts = export_constants()) 
{
    return(100 * saturation_mixing_ratio(P, Td, consts)/saturation_mixing_ratio(P, 
        Temp, consts))
}
