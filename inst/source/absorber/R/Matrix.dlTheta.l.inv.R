Matrix.dlTheta.l.inv = function(dlTheta_l){
  dlTheta_l_tilde_inv= tryCatch({ginv(dlTheta_l)},
                                error= function(e){
                                  r = rankMatrix(dlTheta_l)
                                  svdIrlba = suppressWarnings(irlba::irlba(dlTheta_l, nv = r))
                                  U = svdIrlba$u ; V = svdIrlba$v
                                  D_tilde_inv = diag(c(1/svdIrlba$d))
                                  return(round(V %*% D_tilde_inv %*% t(U), 10))
                                },
                                warning= function(w){
                                  r = rankMatrix(dlTheta_l)
                                  svdIrlba = suppressWarnings(irlba::irlba(dlTheta_l, nv = r))
                                  U = svdIrlba$u ; V = svdIrlba$v
                                  D_tilde_inv = diag(c(1/svdIrlba$d))
                                  return( round(V %*% D_tilde_inv %*% t(U), 10))
                                })

  return(dlTheta_l_tilde_inv)
  
}