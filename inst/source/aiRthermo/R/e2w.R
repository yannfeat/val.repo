e2w <-
function (eh2o, P, consts = export_constants()) 
{
    return(as.double(consts["epsilon"] * eh2o/(P - eh2o)))
}
