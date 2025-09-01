rh2shum <-
function (P, Temp, rh, consts = export_constants()) 
{
    w = rh2w(P, Temp, rh, consts)
    return(w/(1 + w))
}
