get.selected.covariables = function(GL, dlTheta_tilde_inv, Theta_tot, y, n_train, d, all.variables){

  dimSelec = list() ; rss_list = c() ; 
  
  ## for each lambda
  for (i in 1:ncol(GL$beta)){
    beta = GL$beta[,i] ;  b0.lambda = GL$b0[i] ; dimSelec.i = c() ;
    gamma_tot = matrix(nrow = 0, ncol=1)
    
    for (l in 1:d){
      alpha_l = as.matrix(beta[((l-1)*n_train + 1): (l*n_train)])
      gamma_l = dlTheta_tilde_inv[[l]] %*% alpha_l

      gamma_tot = rbind(gamma_tot, gamma_l)
      idx.selec.l = which(round(gamma_l, 10) != 0)
      
      if (length(idx.selec.l) != 0 ) dimSelec.i = c(dimSelec.i, all.variables[l])

    }
    dimSelec[[i]] = dimSelec.i ; 
    Yhat_lambda = Theta_tot %*% gamma_tot
    
    rss = sum((y - (b0.lambda + Yhat_lambda))**2)
    rss_list = c(rss_list, rss)
  }
  return(list(dimSelec, rss_list))
}