parcelState <-
function (Press, Temp, w = 0, consts = export_constants()) 
{
    parcel = list(pressure = Press, temperature = Temp, mixingRatio = w, 
        theta = PT2Theta(Press, Temp, w, consts), virtualTemperature = virtual_temperature(Press, 
            Temp, w, consts), saturationMixingRatio = saturation_mixing_ratio(Press, 
            Temp, consts))
    return(parcel)
}
