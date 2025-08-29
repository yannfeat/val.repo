Matrix.Bspline = function(d, M, knotSelec, x_train, x_min, x_max){

  Psi_obs_List =  lapply(1:d, function(i) bsplineS(x_train[,i], norder = M, breaks=c(x_min[i],knotSelec[[i]], x_max[i]), returnMatrix = T))
  Psi_obs = list()

  idx.obs = list() ; nbSequence = c()
  for (i in 1:d){
    idx.obs[[i]] = which(colSums(Psi_obs_List[[i]] != 0) > 0)
    Psi_obs[[i]] = Psi_obs_List[[i]][, idx.obs[[i]]]
  }
 
  return(list(Psi_obs, idx.obs))
}
  