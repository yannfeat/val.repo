# This is one of the main functions. This function computes the proposed AHM model.
# It needs to tell clearly what are the major components
# under each major component, how many minor components are there
# assuming less than 10 minor components  under each major
#' This is one of the main functions. The function ahm computes the proposed additive heredity model.
#' @param x data.frame Note the column names of the x should be in the order of major components, minor components, and no interactions are needed.
#' @param y numeric vector
#' @param num_major number of major components
#' @param dist_minor the allocation of number of minor components nested under major components
#' @param type heredity type, weak heredity is the current support type
#' @param lambda_seq a numeric vector for the options of lambda used in ridge regression for estimating the initials
#' @param mapping_type the form of the coefficient function of major components in front of corresponding minor terms. Currently only support "power"
#' @param powerh the power parameter used for the power function
#' @param rep_gcv the number of choices of tuning parameter used in the GCV selection
#' @param nfolds used in cv.glmnet for initial value of parameters in the non-negative garrote method
#' @param alpha 0 is for the ridge in glmnet https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
#' @import glmnet quadprog
#' @return Return a list
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' out = ahm (y, x, num_major = 3, dist_minor = c(2,2,1),
#'            type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
#'            mapping_type = c("power"), powerh = h_tmp,
#'            rep_gcv=100)
#' summary(out)

ahm = function(y, x, num_major = 3, dist_minor = c(2,2,1),
               type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfolds=NULL,
               mapping_type = c("power"), powerh = 0,
               rep_gcv=100){

  call <- match.call()

  # store original y, and x with interactions augmented
  x_orig = x
  x_aug_orig = augment_df (x, num_major, dist_minor)
  y_orig = y

  x = mapping_function (x, num_major, dist_minor, mapping_type, powerh) # add mapping function in front of the minor components
  x_aug0 = augment_df (x, num_major, dist_minor)

  # scale x
  ## uncentered data in scale: x_aug
  x_aug=scale(as.matrix(x_aug0),center=FALSE,scale = apply(as.matrix(x_aug0), 2, sd, na.rm = TRUE))
  y=scale(as.matrix(y),center=FALSE,scale = apply(as.matrix(y), 2, sd, na.rm = TRUE))

  # update 05182018
  # zero variance columns return NaN
  # https://stackoverflow.com/questions/15363610/why-does-scale-return-nan-for-zero-variance-columns
  x_aug[is.nan(as.matrix(x_aug))] <- 0
  x_aug = data.frame(x_aug)

  colname = colnames(x_aug)
  num_obs = nrow(x_aug)
  if (is.null(nfolds)) nfolds = num_obs # LOOCV

  # use RIDGE for the initial values
  # LOOCV in glmnet
  cv.RIDGE1<-suppressWarnings( cv.glmnet(as.matrix(x_aug),y=y,alpha=alpha,family='gaussian',intercept=FALSE,nfolds=nfolds,lambda = lambda_seq) )
  # plot(cv.RIDGE1,xvar="lambda")
  coef.RIDGE1=coef(cv.RIDGE1, s = "lambda.min")
  # best_lambda.RIDGE1 <- cv.RIDGE1$lambda.min
  # Convert coef from sparse matrix to normal one
  coef.RIDGE1=Matrix(coef.RIDGE1, sparse = FALSE)
  #
  # construct the quad optim
  # details can be found in the package vignnet
  B=diag((as.vector(coef.RIDGE1))[-1])
  Z=as.matrix(x_aug)%*%B
  D=t(Z)%*%Z
  # matrix D in qp is required to be pd
  # nearPD computes the nearest positive definite matrix.
  D=Matrix::nearPD(D)$mat

  d=t(Z)%*%y

  num_coef = nrow(B)
  A=cbind(-1,diag(num_coef))
  if (type == "weak"){
    con = generate_con_weak (x_aug, B, num_major, dist_minor)
  } else {
    # type == "strong"
    # con = generate_con_strong (x_aug, B, num_major, dist_minor)
  }

  A=cbind(A,t(con))

  b0_min=0
  M_min_set=b0_min+0.01
  # M_min_set is chosen large enough in case of constraints inconsistent

  M=seq(M_min_set,ncol(B),length=rep_gcv)
  gcv=numeric(rep_gcv)
  beta_hat=matrix(rep(0,num_coef*rep_gcv),nrow=num_coef, ncol=rep_gcv) # 15 is because there is 15 parameters in b
  for(i in 1:rep_gcv)
  {
    b0=c(-M[i],rep(0,ncol(A)-1))
    # b0=c(-M[i],rep(b0_min,9),rep(0,6),rep(0,ncol(A)-15-1))
    coef.nng=solve.QP(D,d,A,b0)$sol
    beta.nng=B%*%coef.nng
    beta_hat[,i]=beta.nng
    e=y-Z%*%coef.nng
    # gcv[i]=sum(e^2)/(length(y)*(1-t(diag(w))%*%coef.nng/length(y))^2)
    gcv[i]=sum(e^2)/(length(y)*(1-M[i]/length(y))^2)
  }

  M_min=M[which.min(gcv)]
  b0=c(-M_min,rep(0,ncol(A)-1))
  coef.nng=round(solve.QP(D,d,A,b0)$sol,10) # 10 is round digits
  names(coef.nng)=colname
  # coef.nng
  beta.nng=B%*%coef.nng
  row.names(beta.nng)=colname

  coef_table_scaled=round(cbind(coef.RIDGE1[-1,1],beta.nng,as.matrix(coef.nng)),7) # 7 is round digits
  colnames(coef_table_scaled)= c('Ridge_initial','Theta_nng','Scalor_b')
  e=y-Z%*%coef.nng

  # metrics
  # beta.nng.orig=colSdApply(as.matrix(y_orig, ncol=1))*diag(1/as.vector(colSdApply(x_aug_orig)))%*%beta.nng
  beta.nng.orig=colSdApply(as.matrix(y_orig, ncol=1))*diag(1/as.vector(colSdApply(x_aug0)))%*%beta.nng  # update Dec 08, 2018

  # update 05182018
  # due to the D-optimal design points selected, some coefficients are zero
  # https://stackoverflow.com/questions/15363610/why-does-scale-return-nan-for-zero-variance-columns
  beta.nng.orig[is.nan(as.matrix(beta.nng.orig))] <- 0

  # e.orig=as.matrix(y_orig,ncol=1)-as.matrix(x_aug_orig,nrow=num_obs)%*%beta.nng.orig
  e.orig=as.matrix(y_orig,ncol=1)-as.matrix(x_aug0,nrow=num_obs)%*%beta.nng.orig # update Dec 08, 2018

  mse=drop(t(e.orig)%*%e.orig/(num_obs-sum(beta.nng.orig!=0)))
  # sst=drop(t(as.matrix(y_orig))%*%as.matrix(y_orig) - (sum(as.matrix(y_orig)))^2/num_obs)
  sst=drop(t(as.matrix(y_orig))%*%as.matrix(y_orig))
  # https://stats.stackexchange.com/questions/26176/removal-of-statistically-significant-intercept-term-increases-r2-in-linear-mo
  R2=drop(1-t(e.orig)%*%e.orig/sst)

  # AIC
  rss=drop(t(e.orig)%*%e.orig)
  effective_num_pred = sum(beta.nng.orig!=0) + 1 # one is for the parameter sigma
  aicc = compute_aicc (rss=rss, n=num_obs, p=effective_num_pred, type = "AICc")
  #
  rownames(beta.nng.orig) = colname
  # output
  # At = t(A)
  quad_prog = enlist(D, d, A, b0, M_min)
  x_aug_including_mapping_coef = x_aug0
  out = c(list(beta_nng=beta.nng.orig, residual= e.orig)
          , enlist( mse, R2, aicc, coef_table_scaled, quad_prog, x_aug_including_mapping_coef, x_aug_orig, y_orig, powerh, num_major, dist_minor, mapping_type, call))
  # note, beta_nng is corresponding to the data with mapping function*minor components
  #out = NULL
  class(out) = "ahm"
  return(out)
}

#' Summary method for the fitted ahm object
#'
#' @param object fitted ahm object
#' @param ... not used
#' @rdname summary.ahm
#' @method summary ahm
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' out = ahm (y, x, num_major = 3, dist_minor = c(2,2,1),
#'            type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
#'            mapping_type = c("power"), powerh = h_tmp,
#'            rep_gcv=100)
#' summary(out)
#'
#'
summary.ahm = function(object,...) {
  print(object$call)
  cat("\n")
  cat("The mse of model is ")
  print(object$mse)
  cat(",")
  cat("\n")
  cat("The AICc of model is ")
  print(object$aicc)
  cat(",")
  cat("\n")
  cat("The R2 of model is ")
  print(object$R2)
  cat(",")
  cat("\n")
  cat("The estimated coefficients are: \n")
  print(t(object$beta_nng))
  cat("\n")
  cat("If power function as the coefficients were used, the power parameter, h, used in the model is ")
  print(object$powerh)
}

#' Coefficient method for the fitted ahm object
#'
#' @param object ahm object
#' @param ... not used
#' @return a numerical vector
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' out = ahm (y, x, num_major = 3, dist_minor = c(2,2,1),
#'            type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
#'            mapping_type = c("power"), powerh = h_tmp,
#'            rep_gcv=100)
#' coef(out)
#'
coef.ahm = function(object, ...){
  object$beta_nng
}

#' Predict method for the fitted ahm object
#'
#' @param object ahm object
#' @param newx Matrix of new values for x at which predictions are to be made.
#' @param ... not used
#' @return predicted value(s) at newx
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' out = ahm (y, x, num_major = 3, dist_minor = c(2,2,1),
#'            type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
#'            mapping_type = c("power"), powerh = h_tmp,
#'            rep_gcv=100)
#' predict(out)
#'
predict.ahm = function(object, newx, ...){
  if (0) {
   # object = out
   # newx = x[2,]
   # predict(out)
   # predict(out, newx = x[2,])
  }
  if (missing(newx)) {
    newx_aug = object$x_aug_including_mapping_coef
  } else {
    if (class(newx) == "numeric") {
      newx = as.data.frame(newx)
    }
    num_col = ncol(newx)
    #name = colnames(newx)

    if (num_col != (object$num_major +  sum(ifelse(object$dist_minor==1, 0, object$dist_minor))  )) {
      message("Error: The provided newx does not satify the format of predict matrix. \n")
      message("The format of predictor matrix is: major component + minor components nested under each major components. \n")
      message("For example, c1, c2, c3, x11, x12, x21,x22,x23, x31, x32.")
    } else {
      powerh = object$powerh
      num_major = object$num_major
      dist_minor = object$dist_minor
      mapping_type = object$mapping_type
      # apply the mappiing functions in front of the minor components
      newx = mapping_function (newx, num_major, dist_minor, mapping_type, powerh)

      # construct the interaction terms
      newx_aug = augment_df (newx, num_major, dist_minor)
    }

  }

  out = newx_aug %*% coef(object)
  return(out)
}


#' Mapping_function is a function to add the functional coefficients of major components in front of minor components terms
#'
#' @param x data.frame Note the column names of the x should be in the order of major components, minor components, and no interactions are needed.
#' @param num_major number of major components
#' @param dist_minor the allocation of number of minor components nested under major components
#' @param mapping_type the form of the coefficient function of major components in front of corresponding minor terms. Currently only support "power"
#' @param powerh the power parameter used for the power function
#' @return data frame
#' @export
#' @examples
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' mapping_function(x=x, num_major=3, dist_minor=c(2,2,1), mapping_type = c("power"), powerh=0)


mapping_function = function(x, num_major = 3, dist_minor = C(2,2,1), mapping_type = c("power"), powerh = 0){
  # h is the tuning parameter c^{h} for the transformation
  # 0 < h <= 2
  if (0) {
    powerh=0.5
  }

  # type = match.arg(type)
  colname = colnames(x)

  index_major = seq(from = 1, to = num_major, by=1)
  index_minor = seq(from = (index_major[length(index_major)]+1), to = (num_major+sum(ifelse(dist_minor==1, 0, dist_minor))), by = 1)

  if (mapping_type == "power") {
    # for each minor component
    # multiple the mappling power function c^h
    for (k in index_minor) {
      corresponding_minor_element = colname[k]
      corresponding_minor_element = unlist(strsplit(corresponding_minor_element, "[a-z]+"))[2]
      corresponding_minor_element = floor(as.numeric(as.character(corresponding_minor_element))/10) # assuming less than 10 minor components under each major
      corresponding_major_element = paste("c",corresponding_minor_element,sep='')
      x[,colname[k]] = x[,corresponding_major_element]^powerh * x[,colname[k]]
    }
  }

  return (x)
}

# function colSD
colSdApply <- function(x, ...)apply(X=x, MARGIN=2, FUN=sd, ...)

# generates matrix corresponding to the constraints in quad programming
generate_con_weak = function(x_aug, B, num_major, dist_minor){

  # the pattern of the colume names in x_aug
  # num_major = 3, dist_minor = c(2,2,1)
  # c1 c2 c3 x11 x12 x21 x22 c1c2 c1c3 c2c3 x11x12 x21x22
  index_major = seq(from = 1, to = num_major, by=1)
  index_minor = seq(from = (index_major[length(index_major)]+1), to = (num_major+sum(ifelse(dist_minor==1, 0, dist_minor))), by = 1)
  index_majorbymajor = seq(from = (index_minor[length(index_minor)]+1), to = (index_minor[length(index_minor)] + choose(num_major,2)), by = 1)

  counter_majorbymajor = 0
  for (k in 1:length(dist_minor)) {
    if (dist_minor[k] != 1) {
      counter_majorbymajor = counter_majorbymajor + choose(dist_minor[k],2)
    }
  }
  counter_majorbymajor

  num_major = length(index_major)
  num_minor = length(index_minor)
  num_majorbymajor = length(index_majorbymajor)
  num_minorbyminor = counter_majorbymajor
  index_minorbyminor = seq(from = (index_majorbymajor[length(index_majorbymajor)]+1), to = (index_majorbymajor[length(index_majorbymajor)]+num_minorbyminor))

  num_row_con =  num_majorbymajor + num_minorbyminor + sum(ifelse(dist_minor==1, 0, dist_minor))
  # does not include the component if dist_minor element = 1, which indicates no minor components nested under that major component
  num_col_con = max(index_minorbyminor) #nrow(B) # update Dec 23, 2018
  con_init = matrix(0, nrow=num_row_con, ncol=num_col_con)
  colnames(con_init) = colnames(x_aug)

  # starting from the first row, row-by-row construction
  id_row = 1
  # constraints related to the heredity, focusing on the interaction part in the matrix con
  ## related to the major interaction components
  for (kk in index_majorbymajor) {
    con_init[id_row, kk] = -1
    corresponding_element = unlist(strsplit(colnames(con_init)[kk], "[.]"))
    con_init[id_row, corresponding_element] = 1
    id_row = id_row + 1
  }
  ## related to the minor interaction components, focusing on the minor components part in the matrix con
  for (ll in index_minorbyminor) {
    con_init[id_row, ll] = -1
    corresponding_element = unlist(strsplit(colnames(con_init)[ll], "[.]|[:]"))
    con_init[id_row, corresponding_element] = 1
    id_row = id_row + 1
  }
  # constraints related to the major-minor hierarchical
  for (gg in index_minor) {
    con_init[id_row, gg] = -1
    # https://stat.ethz.ch/pipermail/r-help/2014-July/420407.html
    corresponding_element = unlist(strsplit(colnames(con_init)[gg], "[a-z]+"))[2]
    corresponding_element = floor(as.numeric(as.character(corresponding_element))/10) # assuming less than 10 minor components under each major
    corresponding_element = paste("c",corresponding_element,sep='')
    con_init[id_row, corresponding_element] = 1
    id_row = id_row + 1
  }

  return(con_init)
}



# augment the data frame by adding 2 factor interactions
# based on the ahm model

augment_df = function(  x, num_major = 3,
                        dist_minor = c(2,2,1)){

  if (0) {
    # rm(list=ls())
    # library(devtools)
    # load_all()
    # data("pringles_fat") # to please R CMD CHECK
    x = pringles_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
    y = pringles_fat[,1]

    # the AHM function for general case
    num_major = 3
    dist_minor = c(2,2,1)
    augment_df (x, num_major = 3,
                dist_minor = c(2,2,1)
    )

    # another data
    data_fat_I = NULL # to please R CMD CHECK
    # data("data_fat_I") # to please R CMD CHECK
    x = data_fat_I[,c("c1","c2","c3","x11","x12","x21","x22","x31","x32")]
    y = data_fat_I[, 1]
    num_major = 3
    dist_minor = c(2,2,2)

    augment_df (x, num_major = 3,
                dist_minor = c(2,2,2)
    )

    # another artifical data
    # data("data_fat_I") # to please R CMD CHECK
    x = data_fat_I[,c("c1","c2","c3","x11","x12","x21","x22","x31","x32", "x11x12")]
    x$x11x12 = x$x11x12/2+0.01
    colnames(x)[10]="x33"
    y = data_fat_I[, 1]
    num_major = 3
    dist_minor = c(2,2,3)

    augment_df (x, num_major = 3,
                dist_minor = c(2,2,3)
    )

  }

  dat = x
  names_major = rep(0, num_major)
  names_minor = c() #rep(0, sum(dist_minor[dist_minor>1])) # if minor component = 1, does not count

  # loop over minors over each major components
  for (i in 1:length(dist_minor)) {

    #i=1 # use i as the id for the major components
    names_major[i] <- paste("c", i, sep = "") # names for major components
    num_minor = dist_minor[i] # num of minor nested under that major component
    if (num_minor == 1) {
      # do nothing
      # does not generate the minor when num_minor = 1
    } else {
      ids_minor = seq(from=1, to=num_minor, by=1) # id for the minor components
      names_minor_tmp = paste("x", i,ids_minor, sep = "")
      names_minor <- c(names_minor, names_minor_tmp) # names for minor components
      #
      # # generate the interactions among these minors
      # dat_tmp = dat[,]
    }
  }
  names_minor
  names_major
  names = c(names_major, names_minor)
  assign_colnames = TRUE
  if (assign_colnames) colnames(dat) = names # assign the colnames

  out_dat = dat
  # expand the data set
  ## major
  major_names = grep("c", names, value=TRUE)
  out_dat = cbind(out_dat,expand_interactions (dat, major_names))

  ## minor
  # dist_minor = c(2,3,2)
  for (j in 1:length(dist_minor)) {
    num_minor = dist_minor[j]
    if (num_minor != 1) {
      minor_names = grep(paste("x",j,sep=""), names, value=TRUE)
      expanded_dat = expand_interactions (dat, minor_names)
      out_dat = cbind(out_dat,expanded_dat)
    } else {
      # do nothing
      # no minor components nested
      # do not expand interactions among minors
    }
  }

  out = t(unique(t(out_dat))) # remove duplicated columns resulted from 2fi expansion
  return(out)
}





