# GA + optim hybridization function
hgaoptim = function(genpop, fitfunc, hgaparams,
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
  for(i in rnselpop){
    sol = stats::optim( 
      fn = fitfunc,
      par = genpop[i,1:(m-2)],
      method = hgaparams$method,
      control = hgaparams$control)
    genpop[i,1: (m-2)] = sol$par
    genpop[i,"fitval"] = sol$value
  }
  return(genpop)
}
