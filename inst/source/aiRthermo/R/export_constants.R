export_constants <-
function () 
{
    outvals <- .C("export_constants_Rworld", result = double(12))
    names(outvals[["result"]]) <- c("Rd", "Rv", "T0", "P1000", 
        "es0", "cp", "g", "epsilon", "MISSING_VALUE", "rd_cp", 
        "gamma_dry", "cv")
    outvals[["result"]]
}
