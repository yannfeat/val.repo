#' This is one of the main functions. It perform the cross validation on ahm models to select the optimal setting of hyper parameter h
#'
#' @param x data.frame Note the column names of the x should be in the order of major components, minor components, and no interactions between major or minor components are needed.
#' @param y numeric vector
#' @param metric "mse" or "AICc" the metric used in cross validtion where the minimum is selected as the optimal
#' @param num_major number of major components
#' @param dist_minor the allocation of number of minor components nested under major components
#' @param type heredity type, weak heredity is the current support type
#' @param lambda_seq a numeric vector for the options of lambda used in ridge regression for estimating the initials
#' @param mapping_type the form of the coefficient function of major components in front of corresponding minor terms. Currently only support "power"
#' @param rep_gcv the number of choices of tuning parameter used in the GCV selection
#' @param powerh_path if NULL, then the default is the vector: round(seq(0.001,2,length.out =15),3)
#' @param nfolds used in cv.glmnet for initial value of parameters in the non-negative garrote method
#' @param alpha 0 is for the ridge in glmnet https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
#' @return Return a list
#' @export
#' @examples
#' \donttest{
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' powerh_path = round(seq(0.001,2,length.out =15),3)
#' num_major = 3; dist_minor = c(2,2,1)
#' res = cv.ahm (y, x, powerh_path=powerh_path, metric = "mse", num_major, dist_minor, type = "weak"
#' , alpha=0, lambda_seq=seq(0,5,0.01), nfolds=NULL, mapping_type = c("power"), rep_gcv=100)
#' object = res$metric_mse
#' }

cv.ahm = function(y, x, powerh_path=NULL, metric = c("mse","AICc"), num_major = 3, dist_minor = c(2,2,1),
                  type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfolds=NULL,
                  mapping_type = c("power"),
                  rep_gcv=100){

  # store original y, and x with interactions augmented
  x_orig = x
  x_aug_orig = augment_df (x, num_major, dist_minor)
  y_orig = y


  # cross validation, in the study we use leave one out cross validation
  # also loops over the sequence of h values
  if (is.null(powerh_path)) powerh_path = round(seq(0.001,2,length.out =15),3)
  res_aicc = res_mse = vector(mode = "list", length = length(powerh_path))

  for (ll in 1: length(powerh_path)) {
    h_tmp = powerh_path[ll]
    x_tmp = mapping_function (x, num_major, dist_minor, mapping_type, powerh=h_tmp) # add mapping function in front of the minor components
    x_aug0 = augment_df (x=x_tmp, num_major, dist_minor)

    # the following codes are modified from the package oem
    cv.nfolds = nrow(x)
    foldid = sample(rep(seq(cv.nfolds), length = nrow(x)))
    outlist = as.list(seq(cv.nfolds))
    aicc_path = mscv_path = c()
    for (i in seq(cv.nfolds)) {
      which = foldid == i
      if (is.matrix(y)) {
        y_sub = y[!which,]
      } else {
        y_sub = y[!which]
      }
      outlist[[i]] = ahm (y_sub, x[!which,], num_major, dist_minor,
                          type, alpha, lambda_seq, nfolds,
                          mapping_type, powerh=h_tmp,
                          rep_gcv)
      aicc_path = c(aicc_path, outlist[[i]]$aicc)

      pred = matrix(x_aug0[which,], ncol = ncol(x_aug0)) %*% outlist[[i]]$beta_nng
      mscv = drop(base::crossprod(pred-y[which]))
      mscv_path = c(mscv_path, mscv) # currently only support mse metric
    }
    res_mse[[ll]] = mscv_path
    res_aicc[[ll]] = aicc_path
  }
  res_mse
  res_aicc
  sel_id_mse = which.min(base::sapply(res_mse, mean))
  sel_id_aicc = which.min(base::sapply(res_aicc, mean))
  # base::sapply(res, sd)
  h_tmp_mse = powerh_path[sel_id_mse]
  h_tmp_aicc = powerh_path[sel_id_aicc]

  cvlist_mse = list(cvm = base::sapply(res_mse, mean), cvsd = base::sapply(res_mse, sd), metric = "mse", cvmin = min(base::sapply(res_mse, mean))
                    ,cvmedian = base::sapply(res_mse, median), cvmedian_min = min(base::sapply(res_mse, median)), h_optim= h_tmp_mse, powerh_path = powerh_path, cv.nfolds=cv.nfolds)
  cvlist_aicc = list(cvm = base::sapply(res_aicc, mean), cvsd = base::sapply(res_aicc, sd), metric = "aicc", cvmin = min(base::sapply(res_aicc, mean))
                     ,cvmedian = base::sapply(res_mse, median), cvmedian_min = min(base::sapply(res_mse, median)), h_optim= h_tmp_aicc, powerh_path = powerh_path, cv.nfolds=cv.nfolds)

  # after selecting the optimal h, run ahm on the whole data set
  out = vector(mode = "list", length = 2) # mse and aicc
  names(out) = c("metric_mse", "metric_aicc")
  tmp = ahm (y, x, num_major, dist_minor,
             type, alpha, lambda_seq, nfolds,
             mapping_type, powerh = h_tmp_mse,
             rep_gcv)
  out[[1]] = c(cvlist_mse, tmp)

  tmp = ahm (y, x, num_major, dist_minor,
             type, alpha, lambda_seq, nfolds,
             mapping_type, powerh = h_tmp_aicc,
             rep_gcv)
  out[[2]] = c(cvlist_aicc, tmp)

  class(out) = "cv.ahm"
  return(out)
}


#' Coefficient method for the fitted cv.ahm object
#'
#' @param object cv.ahm object
#' @param metric "mse" or "aicc"
#' @param ... not used
#' @return a numerical vector
#' @export
#' @examples
#' \donttest{
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' powerh_path = round(seq(0.001,2,length.out =15),3)
#' num_major = 3; dist_minor = c(2,2,1)
#' res = cv.ahm (y, x, powerh_path=powerh_path, metric = "mse", num_major, dist_minor, type = "weak"
#' , alpha=0, lambda_seq=seq(0,5,0.01), nfolds=NULL, mapping_type = c("power"), rep_gcv=100)
#' coefficients = coef(res)
#' }
#'
coef.cv.ahm = function(object, metric = "mse", ...) {
  if (0) {
    #object = res
    #coef.cv.ahm(object = res, metric = "mse")
    #coef.cv.ahm(object = res, metric = "aicc")
    #predict.cv.ahm(object = res, metric = "aicc")
  }

  if (metric == "mse") {
    obj = object$metric_mse
  } else if (metric == "aicc"){
    obj = object$metric_aicc
  }
  class(obj) = "ahm"
  coeff = coef(obj)
  return (coeff)
}

#' Predict method for the fitted cv.ahm object
#'
#' @param object cv.ahm object
#' @param metric "mse" or "aicc"
#' @param ... not used
#' @param newx Matrix of new values for x at which predictions are to be made.
#' @return Return a list
#' @export
#' @examples
#' \donttest{
#' data("pringles_fat")
#' data_fat = pringles_fat
#' h_tmp = 1.3
#' x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#' y = data_fat[,1]
#' powerh_path = round(seq(0.001,2,length.out =15),3)
#' num_major = 3; dist_minor = c(2,2,1)
#' res = cv.ahm (y, x, powerh_path=powerh_path, metric = "mse", num_major, dist_minor, type = "weak"
#' , alpha=0, lambda_seq=seq(0,5,0.01), nfolds=NULL, mapping_type = c("power"), rep_gcv=100)
#' pred = predict(res)
#' }
#'

predict.cv.ahm = function(object, newx, metric = "mse", ...) {
  if (metric == "mse") {
    obj = object$metric_mse
  } else if (metric == "aicc"){
    obj = object$metric_aicc
  }
  class(obj) = "ahm"
  out = predict(obj, newx)
  return (out)
}
