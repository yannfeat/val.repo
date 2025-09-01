q2e <-
function (P, q, consts = export_constants()) 
{
    return(as.double(q * P/(consts["epsilon"] * (1 - q) + q)))
}
