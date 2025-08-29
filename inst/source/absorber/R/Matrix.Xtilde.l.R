Matrix.Xtilde.l = function(l, d, n_train, Psi_prim, Psi_obs, idx.obs){

  Psi_prim_red = Psi_prim[[l]][, idx.obs[[l]]]
  
  
  ##dlPhi_l_dot and Phi_l_dot/2 :
  outMat = Matrix.Theta.l(l, d, n_train, Psi_prim_red, Psi_obs)
  dlTheta_l = outMat[[1]] ;  Theta_l = outMat[[2]]
  
  ## Moore-penrose inverse
  dlTheta_l_tilde_inv = Matrix.dlTheta.l.inv(dlTheta_l)
  
  ## product Theta_l x  dlTheta_l_tilde inverted : n x n
  Xl_tilde = Theta_l %*% dlTheta_l_tilde_inv
  
  return(list(Xl_tilde, dlTheta_l_tilde_inv, Theta_l))
}