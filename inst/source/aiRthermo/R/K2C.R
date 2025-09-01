K2C <-
function (Tk, consts = export_constants()) 
{
    return(as.double(Tk - consts["T0"]))
}
