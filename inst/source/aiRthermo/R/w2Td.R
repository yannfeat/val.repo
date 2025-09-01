w2Td <-
function (P, w, consts = export_constants()) 
{
    return(as.double(5417/(19.829999999999998 + log(consts["es0"]) + 
        log(w + consts["epsilon"]) - log(w * P))))
}
