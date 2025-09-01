bruntVaisallaOmegaSquared <-
function (Ps, Ts, ws, consts = export_constants()) 
{
    if ((length(Ps) != length(Ts)) | (length(Ps) != length(ws)) | 
        (length(Ts) < 2)) {
        print("In bruntVaisallaOmegaSquared(), Ps, Ts and ws must be 1D arrays the same length")
        return(NA)
    }
    thetas = PT2Theta(Ps, Ts, ws, consts)
    Nlevs = length(thetas)
    derivatives = rep(0, Nlevs)
    rho = densityMoistAir(Ps, Ts, ws, consts)
    if (Ps[1] > Ps[2]) {
        thesign = 1
    }
    else {
        thesign = -1
    }
    derivatives[1] = (thetas[2] - thetas[1])/(Ps[2] - Ps[1])
    derivatives[Nlevs] = (thetas[Nlevs] - thetas[Nlevs - 1])/(Ps[Nlevs] - 
        Ps[Nlevs - 1])
    derivatives[2:(Nlevs - 1)] = (thetas[3:Nlevs] - thetas[1:(Nlevs - 
        2)])/(Ps[3:Nlevs] - Ps[1:(Nlevs - 2)])
    derivatives = thesign * derivatives
    omega_sq = -rho * consts["g"]^2 * derivatives/thetas
    return(omega_sq)
}
