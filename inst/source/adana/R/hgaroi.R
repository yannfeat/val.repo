# GA + ROI hybridization function
hgaroi = function(genpop, fitfunc, hgaparams,
                  hgaftype, hgans, ...){
  n = nrow(genpop)
  m = ncol(genpop)
  if(missing(hgans)) hgans = 1
  if(hgans > n) hgans = n
  if(missing(hgaftype)) hgaftype="w" 
  if(hgaftype=="b"){ 
    selpop = genpop[order(genpop[,m], decreasing=TRUE),]
    selpop = selpop[1:hgans,]
  }else if(hgaftype=="w"){
    selpop = genpop[order(genpop[,m], decreasing=FALSE),]
    selpop = selpop[1:hgans,]
  }else if(hgaftype=="r"){
    selpop = genpop[sample(1:n, size=hgans, replace=FALSE),]
  }
  newpop = matrix(NA, nrow=hgans, ncol=m)
  rnselpop = rownames(selpop)
  #require(ROI)
  fo = F_objective(F=fitfunc, n=(m-2))
  vb = V_bound(li=1: (m-2), ui=1: (m-2), 
               lb=hgaparams$lower, ub=hgaparams$upper)
  op = OP(objective=fo, bounds = vb, maximum=TRUE)
  for(i in rnselpop){
    sol = ROI_solve(op, 
                    solver="optimx", 
                    control=hgaparams$control,
                    start=genpop[i,1:(m-2)])
    genpop[i,1:(m-2)] = sol$solution
    genpop[i,m] = sol$objval
  }
  return(genpop)
}