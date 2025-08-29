absorber = function(x, y, M = 3, K = 1, all.variables = NULL, parallel = FALSE, nbCore = 1){
  
  n_train = nrow(x) ; d = ncol(x) ; 
  y = transformation.magnitude(y)$utrans
  
  if (is.null(all.variables)) all.variables = 1:d
    
  x_min = rep(0,d) ; x_max = rep(1,d) ;
  min_knot_possible = (x_max-x_min)/20000 ; max_knot_possible = x_max - (x_max-x_min)/20000 
    
  ## Selection of evenly spaced knots
  knotSelec = list()
  for (i in 1:d){
    knotSelec[[i]] = seq(from=min_knot_possible[i], to=max_knot_possible[i], length.out=K)
  }
  
  ## B-splines derivatives Matrix
  Psi_prim = lapply(1:d, function(i) bsplineS(x[,i], norder = M, breaks=c(x_min[i], knotSelec[[i]], x_max[i]), returnMatrix = T, nderiv = 1))
  outMat = Matrix.Bspline(d, M, knotSelec, x, x_min, x_max)
  Psi_obs = outMat[[1]] ; idx.obs = outMat[[2]] ;
  
  ### building X prim
  if (parallel == T) outFun = mclapply(1:d, function(l) Matrix.Xtilde.l(l, d, n_train, Psi_prim, Psi_obs, idx.obs), mc.preschedule = T, mc.cores = nbCore) else outFun = lapply(1:d, function(l) Matrix.Xtilde.l(l, d, n_train, Psi_prim, Psi_obs, idx.obs)) ;
  
  X_tilde = do.call('cbind', lapply(outFun, `[[`, 1)) ;
  dlTheta_tilde_inv = lapply(outFun, `[[`, 2) ;
  Theta_tot = do.call('cbind', lapply(outFun, `[[`, 3)) ;
  rm(outFun) ; invisible(gc()) ; 
  
  ## GROUP-LASSO BY USING SPARSEGL -- asparse = 0 for only GROUP-LASSO and not LASSO
  nbSequence = rep(1:d, each = n_train)
  GL = sparsegl(x=X_tilde, y=y, group = nbSequence, asparse = 0)
  
  ## get selected variables
  outFun = get.selected.covariables(GL, dlTheta_tilde_inv, Theta_tot, y, n_train, d, all.variables)
  dimSelec = outFun[[1]] ; RSS = outFun[[2]] ;
  rm(outFun) ; invisible(gc()) ; 
  
  ## using AIC to select lambda
  n_variables =lengths(dimSelec)
  aic = n_train*log(RSS/n_train)+ 2*(n_variables*(K+M) + n_variables*(n_variables-1)*(K+M)^2)
  dim.aic = dimSelec[[which.min(aic)]]

  return(list(lambdas = GL$lambda, selec.var = dimSelec, aic.var = dim.aic))
}
  
