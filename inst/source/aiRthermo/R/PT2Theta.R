PT2Theta <-
function (P, Temp, w = 0, consts = export_constants()) 
{
    rdcp = consts["Rd"]/moistCp(w, consts)
    return(as.double(Temp * (consts["P1000"]/P)^rdcp))
}
