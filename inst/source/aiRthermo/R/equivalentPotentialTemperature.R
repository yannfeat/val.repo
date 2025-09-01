equivalentPotentialTemperature <-
function (P, Temp, w, TLCL, consts = export_constants()) 
{
    theta = PT2Theta(P, Temp, w, consts)
    kd = 0.28539999999999999
    thetaDL = theta * ((theta/TLCL)^(0.28000000000000003 * w) * 
        (1 + w/consts["epsilon"])^kd)
    L0star = 2563130
    L1star = 1754
    Lstar = L0star + L1star * (TLCL - consts["T0"])
    cpd = 1005.7
    k2 = 1137000
    thetaE = thetaDL * exp((Lstar + k2 * w) * w/(cpd * TLCL))
    return(as.double(thetaE))
}
