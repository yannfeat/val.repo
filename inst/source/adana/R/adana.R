# General Objective Adaptive GA Optimization Function
adana = function(gatype="gga", objective="max", maxiter=100, 
                 initfunc=initbin, fitfunc, selfunc=seltour,
                 crossfunc=px1, mutfunc=bitmut, replace=elitism,
                 adapfunc=NULL, hgafunc=NULL, monitorfunc=NULL,
                 n=100, m=8, lb=rep(0,8), ub=rep(1,8), nmode="real", type=1,
                 permset=0:9, prevpop=NULL, 
                 selt=2, selbc=0.5, selc=2, selk=1.005, sells=1.5, 
                 selns=0.5, selps=0.5, sels=1.5, selt0=50, selw=2, 
                 reptype=FALSE, cxpc=0.9, cxpc2=0.8, cxon=2, cxk=2, 
                 cxps=0.5, cxa=0, cxb=0.15, cxealfa=1, cxalfa=0.5,
                 mutpm=0.05, mutpm2=0.2, mutb=2, mutpow=2, mutq=0.5,
                 mutmy=c(), mutsdy=c(), adapa=0.75, adapb=0.5,
                 adapc=0.1, adapd=0.1, hgastep=10, hgans=1, hgaftype="w",
                 reps=1, repk=10, lambda=1, tercrit=c(1), 
                 abdif=1e-06, bestdif=1e-06, objval=0, optdif=1e-06,
                 rmcnt=10, rmdif=1e-06, phidif=1e-06, rangedif=1e-06,
                 meandif=1e-06, sddif=1e-06, mincv=0.001, simlev=0.95,
                 maxtime=60, keepbest = TRUE, parfile=NULL, verbose=FALSE, ...){
  dotargs = list(...)
  # Upload parameters file if available
  if(!is.null(parfile)) source(parfile)
  # Initial population
  initpop=initialize(initfunc, n=n, m=m, lb=lb, ub=ub,
                     permset=permset, nmode=nmode, prevpop=prevpop, type=1, dotargs)
  genpop = initpop
  genfits = matrix(NA, nrow=maxiter, ncol=8)
  colnames(genfits) = c("min","max", "mean", "sd",
                        "Q1", "Q2", "Q3", "cv%")
  stime = Sys.time()
  g = 1
  tcode = 0
  bestsol = list(chromosome=c(), fitval=-Inf, generation=0)
  while(tcode==0 & g<=maxiter){
    genpop[,m+2] = evaluate(fitfunc, genpop[,1:m], objective)
    genfits[g,1] = min(genpop[,m+2])
    genfits[g,2] = max(genpop[,m+2])
    genfits[g,3] = mean(genpop[,m+2])
    genfits[g,4] = sd(genpop[,m+2])
    genfits[g,5] = quantile(genpop[,m+2])[2]
    genfits[g,6] = quantile(genpop[,m+2])[3]
    genfits[g,7] = quantile(genpop[,m+2])[4]
    genfits[g,8] = round(genfits[g,4]/genfits[g,3]*100,2)
    if(!missing(tercrit))
      tcode = terminate(tercrit=tercrit, maxiter=maxiter,
                        objective=objective, t=g, genfits=genfits, 
                        fitvals=genpop[,m+2], objval=objval, optdif=optdif,
                        rmcnt=rmcnt, rmdif=rmdif, abdif=abdif, mincv=mincv,
                        sddif=sddif, rangedif=rangedif, phidif=phidif,
                        simlev=simlev, meandif=meandif, bestdif=bestdif,
                        stime=stime, maxtime=maxtime)
    fitvals = genpop[,ncol(genpop)]
    if(keepbest){
      if(bestsol$fitval < max(fitvals)){
        bidx = which.max(fitvals)[1]
        bestsol$chromosome = genpop[bidx, 1:m]
        bestsol$fitval = genpop[bidx, m+2]
        bestsol$generation = g
      }
    }
    if(!is.null(adapfunc)){  # Adaptation
      ocxpc = cxpc; omutpm = mutpm
      adappar = adapfunc(fitvals=fitvals, n=n, g=g, 
                         gmax=maxiter, cxpc=cxpc, cxpc2=cxpc2, mutpm=mutpm,
                         mutpm2=mutpm2, adapa=adapa, adapb=adapb,
                         adapc=adapc, adapd=adapd)
      cxpc = adappar$pc ; mutpm = adappar$pm 
    }
    selidx = select(selfunc, fitvals, selt=selt)
    matpool = genpop[selidx,]
    shfidx = sample(1:nrow(matpool)) 
    matpool = matpool[shfidx,] # Shuffle the mating pool 
    reppars = shfidx[1:2]      # Mark first two individuals for SSGA 
    offsprings = cross(crossfunc, matpool=matpool,
                       cxon=cxon, cxpc=cxpc, lb=lb, ub=ub,
                       gatype=gatype, cxk=cxk, cxps=cxps, 
                       cxa=cxa, cxb=cxb, dotargs)
    if(is.vector(offsprings)) 
      offsprings = matrix(offsprings, nrow=1, ncol=m)
    mutoffsprings = mutate(mutfunc=mutfunc, population=offsprings, 
                           mutpm=mutpm, gatype=gatype, lb=lb, ub=ub, 
                           mutmy=mutmy, mutsdy=mutsdy, mutb=mutb, mutpow=mutpow,
                           g=g, gmax=maxiter, dotargs)
    if(is.vector(mutoffsprings))
      mutoffsprings = matrix(mutoffsprings, nrow=1, ncol=m) 
    rownames(mutoffsprings) = paste0("T",g, ".",
                                     1:nrow(mutoffsprings))
    mutoffsprings[, m+1] = -1
    mutoffsprings[, m+2] = evaluate(fitfunc, 
                                    mutoffsprings[,1:m], objective)
    genpop = replace(parpop=genpop, offpop=mutoffsprings, 
                     repk=repk, reps=reps, reppars=reppars)
    genpop[, (m+1)] = genpop[, (m+1)]+1
    if(!is.null(hgafunc) & g%%hgastep==0) # Hybridization
      genpop = hgafunc(genpop, fitfunc=fitfunc,
                       hgaparams=dotargs$hgaparams, hgaftype=hgaftype, 
                       hgans=hgans, dotargs)
    if(verbose) cat("Generation ", g,":", genfits[g,], "\n")
    if(g>1 & !is.null(monitorfunc)){
      x = bestsol$chromosome
      show(monitorfunc, g=g, genfits=genfits, 
           objective=objective, x=x, dotargs)
    }
    if(!is.null(adapfunc)){
      cxpc = ocxpc
      mutpm = omutpm
    }
    g = g+1
  }
  genfits = genfits[!is.na(genfits[,1]),]
  if(is.element(tcode, c(4,7)))
    genfits=genfits[1:nrow(genfits)-1,]
  if(objective=="min"){
    if(!is.matrix(genfits)) genfits = matrix(genfits, nrow = 1,ncol = 8)
    genfits[,c(1:3,5:8)] = -1*genfits[,c(1:3,5:8)]
    bestsol$fitval = -1*bestsol$fitval
  }
  #cat("Termination criterion:", tcode,"\n")
  return(list(genfits=genfits, initpop=initpop, finalpop=genpop, 
              bestsol=bestsol, objective=objective, tcode=tcode)) 
}