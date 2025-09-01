C2K <-
function (Tc, consts = export_constants()) 
{
    return(as.double(Tc + consts["T0"]))
}
