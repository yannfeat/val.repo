virtual_temperature <-
function (P, Temp, w, consts = export_constants()) 
{
    partial_press = q2e(P, w2q(w), consts)
    Tv = Temp/(1 - (partial_press/P) * (1 - consts["epsilon"]))
    return(as.double(Tv))
}
