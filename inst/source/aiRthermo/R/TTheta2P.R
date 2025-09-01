TTheta2P <-
function (Temp, Theta, w = 0, consts = export_constants()) 
{
    cprd = moistCp(w, consts)/consts["Rd"]
    return(as.double(consts["P1000"] * ((Temp/Theta)^cprd)))
}
