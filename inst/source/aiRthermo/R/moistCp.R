moistCp <-
function (w, consts = export_constants()) 
{
    return(as.double(consts["cp"] * (1 + 0.87 * w2q(w))))
}
