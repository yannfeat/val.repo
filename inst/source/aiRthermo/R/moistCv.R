moistCv <-
function (w, consts = export_constants()) 
{
    return(as.double(consts["cv"] * (1 + 0.96999999999999997 * 
        w2q(w))))
}
