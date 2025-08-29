Matrix.Theta.l = function(l, d, n_train, Psi_prim_red, Psi_obs){


  ##dlPhi_l_dot and Phi_l_dot/2 :
  dPhi_l_dot = matrix(nrow = n_train, ncol=0) ; Phi_l_dot_half = matrix(nrow = n_train, ncol=0)
  
  if (l < d){
    for (j in (l+1):d){
      dPhi_l_j = face_split(Psi_prim_red, Psi_obs[[j]])
      dPhi_l_dot = cbind(dPhi_l_dot, dPhi_l_j)
      
      ###############################################
      Phi_l_j_half = face_split(Psi_obs[[l]], Psi_obs[[j]])/2
      Phi_l_dot_half = cbind(Phi_l_dot_half, Phi_l_j_half)
    }
  }
  
  
  ##dlPhi_dot_l and Phi_dot_l/2 :
  dPhi_dot_l = matrix(nrow = n_train, ncol=0) ; Phi_dot_l_half = matrix(nrow = n_train, ncol=0)
  
  if (l > 1){
    for (j in 1:(l-1)){
      dPhi_j_l = face_split(Psi_obs[[j]], Psi_prim_red)
      dPhi_dot_l = cbind(dPhi_dot_l, dPhi_j_l)
      
      ###############################################
      Phi_j_l_half = face_split(Psi_obs[[j]], Psi_obs[[l]])/2
      Phi_dot_l_half = cbind(Phi_dot_l_half, Phi_j_l_half)
    }
  }
  
  
  ## dlTheta_l and Theta_l :
  dlTheta_l = cbind(Psi_prim_red, dPhi_l_dot, dPhi_dot_l)
  Theta_l = cbind(Psi_obs[[l]], Phi_l_dot_half, Phi_dot_l_half)
  
  return(list(dlTheta_l, Theta_l))
  
  }
