PTheta2T <-
function (P, Theta, w = 0, consts = export_constants()) 
{
    rdcp = consts["Rd"]/moistCp(w, consts)
    return(as.double(Theta * (P/consts["P1000"])^rdcp))
}
