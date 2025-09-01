saturation_mixing_ratio <-
function (P, Temp, consts = export_constants()) 
{
    es = saturation_pressure_H2O(Temp)
    return(as.double(consts["epsilon"] * es/(P - es)))
}
