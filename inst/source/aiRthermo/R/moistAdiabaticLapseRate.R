moistAdiabaticLapseRate <-
function (w, consts = export_constants()) 
{
    return(as.double(consts["gamma_dry"]/(1 + 0.87 * w)))
}
