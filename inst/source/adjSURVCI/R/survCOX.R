#' Stratified Marginal Proportional Hazards Model For Clustered Survival Data 
#' @description  Stratified marginal proportional hazards model for clustered survival data.
#' The estimates of the cumulative baseline hazard along with their standard errors are provided at the
#' pre-specified time points.
#' Furthermore, the estimated adjusted survival probabilities along with their standard errors are calculated at pre-specified time points. The standard errors of the
#' difference in estimated adjusted survival probabilities between the groups are also provided.
#' Finally, the estimates of survival probabilities given vector \code{Z0} along with their standard errors are provided at
#' pre-specified time points. Tied data are handled by adding a tiny random shift from a normal distribution with mean 0 and standard deviation
#' 1e-09.
#' @param times Vector of failure/censored times.
#' @param deltas Event indicator with 1 as an event and 0 as censoring.
#' @param covariates Matrix of covariates. For categorical covariates, dummy variable must be created.
#' @param treatment Vector of treatment variable. This is also the strata variable. It is a vector
#' with numeric code for each group or stratum.
#' @param clusters Vector of clustering variable. Independent data are assumed if not provided.
#' @param stratified.model \code{TRUE} or \code{FALSE}. By default, it is \code{TRUE} for stratified model. The stratification variable is \code{treatment}.
#' If this is \code{FALSE} and \code{est.t=TRUE}, then the \code{treatment} variable still needs to be provided and will be used as a covariate.
#' @param est.t \code{TRUE} or \code{FALSE}. By default this is \code{FALSE}. If \code{TRUE} then estimates of cumulative baseline hazard, adjusted survival probabilities and predicted survival probabilities are calculated. 
#' @param pre.t Vector of pre-specified time points at which the standard errors of the cumulative baseline hazard, adjusted survival probabilities and predicted survival probabilities are calculated. 
#' By default these are the time points where main event occurs.
#' @param Z0 Vector of covariates at which predicted survival probabilities are calculated. By default this vector is a zero vector.
#' 
#' @return Returns a list with the following components. If \code{est.t=FALSE} then only upto
#' $nstrata are provided.
#' \item{$coef}{Parameter estimates}
#' \item{$p.value}{p-value of regression coefficients}
#' \item{$var}{Covariance matrix of parameter estimates calculated based on sandwich type variance}
#' \item{$infor}{Information matrix}
#' \item{$loglikelihood}{Maximum log-likelihood value}
#' \item{$n}{Total number of observations used}
#' \item{$nevents}{Total number of events and censored observations}
#' \item{$nclusters}{Total number of clusters}
#' \item{$nstrata}{Total number of treatment groups}
#' \item{$CumBaseHaz.t}{Cumlative baseline hazard estimates and their standard errors}
#' \item{$Spredict.t}{Predicted survival probabilities and their standard errors}
#' \item{$AdjustedS.t}{Adjusted survival probabilities and their standard errors}
#' \item{$Adjusted.se.diff}{Standard error of the difference of adjusted survival probabilities between the treatment groups}
#' @export
#' @examples
#' #Simulated data 
#' alpha = 0.5
#' d = simulate_surv_data(N=100,alpha=alpha,beta1=0.5*1/alpha,beta2=-0.5*1/alpha,
#' beta3=1/alpha,rateC=1.3,lambda0=1,lambda1=2,stratified = TRUE)
#' 
#' #Stratified Model with est.t=TRUE
#' model1 <- survCOX(times=d$times,deltas=d$delta,covariates=d[,5:7],treatment=d[,8],
#' clusters=d$cluster,est.t=TRUE,pre.t=sort(d$times[d$delta==1]),Z0=c(1,0.5,1) )
#' 
#' #Unstratified Model with est.t=TRUE
#' model2 <- survCOX(times=d$times,deltas=d$delta,covariates=d[,5:7],treatment=d[,8],
#' clusters=d$cluster,est.t=TRUE,pre.t=sort(d$times[d$delta==1]),stratified.model=FALSE,
#' Z0=c(1,0.5,1) )
#' 
#' #Stratified Model with est.t=FALSE
#' model3 <- survCOX(times=d$times,deltas=d$delta,covariates=d[,5:7],treatment=d[,8],
#' clusters=d$cluster,est.t=FALSE,pre.t=sort(d$times[d$delta==1]),Z0=c(1,0.5,1) )
#' 
#' #Unstratified Model with est.t=FALSE
#' model4 <- survCOX(times=d$times,deltas=d$delta,covariates=cbind(d[,5:7],d[,8]),
#' clusters=d$cluster,est.t=FALSE,pre.t=sort(d$times[d$delta==1]),
#' stratified.model=FALSE,Z0=c(1,0.5,1) )
#' 
#' #Only continuous covariates are available
#' model5 <- survCOX(times=d$times,deltas=d$delta,covariates=d[,5:7],
#' clusters=d$cluster,est.t=FALSE,pre.t=sort(d$times[d$delta==1]),
#' stratified.model=FALSE,Z0=c(1,0.5,1) )

survCOX <- function(times, 
                  deltas, 
                  covariates,
                  treatment=NULL, 
                  clusters=1:length(times), 
                  stratified.model=TRUE,
                  est.t = FALSE,
                  pre.t=sort(times[deltas==1]),
                  Z0=NULL){
  
  ################################### Housekeeping stuff ############################
  if(sum(unique(deltas)>1) > 0){
    stop("deltas should be a vector of 0 and 1 where 1 represent an event")
  }
  
  if(stratified.model==TRUE){
    if(missing(treatment)){
      stop("Please provide treatment when stratified.model=TRUE.")
    }
  }
  
  if(stratified.model==FALSE & est.t==TRUE){
    if(missing(treatment)){
      stop("Please provide treatment when stratified.model=FALSE and est.t=TRUE.")
    }
  }
  
  if(stratified.model==FALSE & length(treatment)>=1 & est.t==FALSE){
    stop("Please don't provide treatment when stratified.model=FALSE and est.t=FALSE.")}
  
  if(is.null(Z0)){Z0=rep(0,ncol(covariates))}
  stratas <- treatment
  lambda.t <- pre.t
  number.of.stratum <- length(unique(stratas))
  
  if(stratified.model==FALSE & est.t==TRUE){ #Unstratified Model for main cause
    stratas.dummy = model.matrix(~factor(stratas))[,-1]
    covariates=cbind(stratas.dummy,covariates)
    if(number.of.stratum==2){
      colnames(covariates)[1] <- paste("Stratum",sort(unique(stratas))[2],sep=" ")
    }
  }
  stratum.names = as.character(sort(unique(stratas)))
  
  if(is.null(clusters)){
    clusters = 1 : length(times)
    cluster.check <- NULL
  }else{
    cluster.check <- "a"
  }
  if(stratified.model==FALSE){
    stratas = rep(1,length(times))
  }

  
  if(is.vector(covariates)){covariates=matrix(covariates)}
  list.variable.names <- colnames(covariates)

  causes <- deltas
  d = data.frame(times,causes,stratas,clusters,covariates)
  
  
  
  #Handling duplicate times by adding a tiny random shift
  id.duplicate = which(duplicated(d$times))
  d[id.duplicate,"times"] = d$times[id.duplicate] + abs(rnorm(length(id.duplicate),0,0.000000001))
  
  d = d[order(d$times),] #Order the data by time
  d = na.omit(d)
  nreturn = nrow(d)
  ereturn = table(d$causes)
  ftime = d$times

  ucl = sort(unique.default(d$clusters))
  cluster = match(d$clusters,ucl)
  creturn = length(unique(cluster))

  covs <- d[,-c(1:4)]
  
  
  if(is.vector(covs)){covs=matrix(covs)}
  
  strata = d$stratas
  sreturn = length(unique(strata))
  ntotal <- length(ftime)
  remove(times,causes,covariates,stratas)
  
  ################################### Housekeeping stuff end #########################
  
  ################################### Optimization ######################################
  b <- rep(0,ncol(covs))
  delta=2
  while(max(abs(delta)) > 10^(-6)){
    I.actual = matrix(0,ncol=ncol(covs),nrow=ncol(covs)); U.actual=matrix(0,nrow=ncol(covs),ncol=1); loglik=0;
    for(i in sort(unique(strata))){
      idx = which(strata==i)
      newd = d[idx,]
      mine = mybetaestCOX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),b)
      I.actual = I.actual + mine[[4]]
      U.actual = U.actual + mine[[3]]
      loglik = loglik + mine[[2]]
    }
    
    b = mine[[1]] + c(solve(I.actual)%*%U.actual)
    delta = c(solve(I.actual)%*%U.actual)
    #print(delta)
  }
  ################################### Optimization end ###################################
  
  ###################################### SE of beta #########################################
  etas <- matrix(NA,ncol(covs),nrow(d))
  Lambdas0 <- list()
  s.idx = 0
  for(s in sort(unique(strata))){
    s.idx = s.idx + 1
    idx = which(strata==s)
    newd = d[idx,]
    
    test = se_beta_strata_COX_survival(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
                                     length(unique(newd$clusters)),solve(I.actual) )
    etas[,idx] <- test[[1]]
    Lambdas0[[s.idx]] <- cbind(time=newd$times,H0=cumsum(c(test[[2]])))
    
  }
  

    # manoj = 0
    # for(i in sort(unique(cluster))){
    #   if(is.null(dim(etas[,cluster==i]))){
    #     temp = etas[,cluster==i]
    #   }else{
    #     temp = apply(etas[,cluster==i],1,sum ) 
    #   }
    #   manoj = manoj + temp %*% t(temp)
    # }
    # var <- solve(I.actual) %*% manoj %*% solve(I.actual)
  
  
  V= 0
  if(is.null(cluster.check)){
    for(j in sort(unique(cluster))){
      #if(is.null(dim(middle[,cluster==j]))){
      temp = etas[,cluster==j]
      #}else{
      #temp = apply(middle[,cluster==j],1,sum ) 
      #}
      #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
      V = V + temp %*% t(temp)
    }
  }else{
    if(ncol(covs)==1){
      for(j in sort(unique(cluster))){
        if(is.null(dim(etas[,cluster==j])) ){
          
          temp = sum(etas[,cluster==j])
        }
        # else{
        #   temp = apply(middle[,cluster==j],1,sum ) 
        # }
        #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
        V = V + temp %*% t(temp)
      }
    }else{
      for(j in sort(unique(cluster))){
        if(is.null(dim(etas[,cluster==j])) ){
          
          temp = etas[,cluster==j]
        }else{
          temp = apply(etas[,cluster==j],1,sum )
        }
        #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
        V = V + temp %*% t(temp)
      }
    }
    
    
  }
  
  var = solve(I.actual) %*% (V) %*% solve(I.actual)
    
    
  ###################################### SE of beta end #####################################
  
  ########################### Individual p-value ###############################################
  #Individual p-value
  ind.p.values <- 1 - (pchisq( (c(b)/sqrt(diag(var)) ) ^2,df=1 ) )
  names(ind.p.values) <- list.variable.names
  colnames(b) <- list.variable.names
  colnames(var) <- list.variable.names
  rownames(var) <-list.variable.names
  
  ########################### Individual p-value end ###############################################
  
if(est.t==TRUE){
  if(stratified.model==TRUE){
    ###################################### SE of lambda0  #####################################
    
    Lambdas0 <- list()
    s.idx = 0
    var.lambda.final <- list()
    var.Spred.final <- list()
    lambda.final <- list()
    var.Sstar.final <- list()
    lambda.final <- list()
    Sstar.t <- list()
    Sstar <- list()
    Zpred <- list()
    W1 <- list()
    W3 <- list()
    for(s in sort(unique(strata))){
      s.idx = s.idx + 1
      idx = which(strata==s)
      newd = d[idx,]
      temp.cluster = cluster[idx]
      
      idx.tlambda = NULL
      for(i in 1 : length(lambda.t)){
        if(lambda.t[i]>=min(newd$times)){
          idx.tlambda[i]=max(which(newd$times<=lambda.t[i]))
        }
        else{
          idx.tlambda[i]=1
        }
      }
      
      
      test = se_lambda_strata_COX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
                                  length(unique(temp.cluster)),solve(I.actual),idx.tlambda,nrow(d),etas,length(unique(cluster)) )
      Lambdas0[[s.idx]] <- cbind(time=newd$times,H0=cumsum(c(test[[2]])))
      
      W1[[s.idx]] <- test[[3]]
      W3[[s.idx]] <- t(test[[4]])
      
      
      
      #W1Sstar <- test[[6]]
      #W3Sstar <- t(test[[7]])
      
      manoj = 0; manoj1=0;
      for(j in 1 : length(unique(cluster))){
        
        manoj = manoj + (colSums(matrix(W1[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                           length(unique(temp.cluster))/length(unique(cluster))*
                           colSums(matrix(W3[[s.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        ## manoj1 = manoj1 + (colSums(matrix(W1Sstar[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
        ##                      length(unique(temp.cluster))/length(unique(cluster))*
        ##                      colSums(matrix(W3Sstar[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        
        
      }
      
      var.lambda.final[[s.idx]] = manoj/(length(unique(temp.cluster)))^2
      
      #var.Sstar.final[[s.idx]] = manoj1/(length(unique(temp.cluster)))^2
      
      lambda.final[[s.idx]] =cbind(time=lambda.t,est=c(test[[5]]),se=sqrt(var.lambda.final[[s.idx]]),stratum=s)
      #Sstar.t[[s.idx]] = cbind(time=lambda.t,est=c(test[[8]]),var=var.Sstar.final[[s.idx]])
      #Sstar[[s.idx]] = cbind(time=newd$times,Star=c(test[[9]]))
      
      
    }
    
    
    
    ###################################### SE of lambda0 end  #####################################
    
    ######################################### Adjusted survival #####################################
    s.idx = 0
    W1Sstar <- list()
    W3Sstar <- list()
    
    for(s in sort(unique(strata))){
      s.idx = s.idx + 1
      idx = which(strata==s)
      newd = d[idx,]
      temp.cluster = cluster[idx]
      
      idx.tlambda = NULL
      for(i in 1 : length(lambda.t)){
        if(lambda.t[i]>=min(newd$times)){
          idx.tlambda[i]=max(which(newd$times<=lambda.t[i]))
        }
        else{
          idx.tlambda[i]=1
        }
      }
      test1 = se_adjusted_strata_COX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
                                     length(unique(temp.cluster)),solve(I.actual),idx.tlambda,nrow(d),etas,length(unique(cluster)),c(lambda.final[[s.idx]][,2]),
                                     data.matrix(covs),b,Z0 )
      ## test1 = se_adjusted_strata_COX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
      ##                   nrow(newd),solve(I.actual),idx.tlambda,nrow(d),etas,nrow(d),c(lambda.final[[s.idx]]),
      ##                  data.matrix(covs) )
      W1Sstar[[s.idx]] <- test1[[1]]
      W3Sstar[[s.idx]] <- t(test1[[2]])
      
      W3pred <- t(test1[[5]]); W3predfirst <- test1[[6]]; Spred <- test1[[4]];
      
      manoj1=0;manojpred=0;
      for(j in 1 : length(unique(cluster))){
        manoj1 = manoj1 + (colSums(matrix(W1Sstar[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                             length(unique(temp.cluster))/length(unique(cluster))*
                             colSums(matrix(W3Sstar[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        manojpred = manojpred + (colSums(matrix(W1[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                                   length(unique(temp.cluster))/length(unique(cluster))*
                                   colSums(matrix(W3pred[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
      }
      
      
      var.Sstar.final[[s.idx]] = manoj1/(length(unique(temp.cluster)))^2
      var.Spred.final[[s.idx]] = manojpred * (W3predfirst)^2/(length(unique(temp.cluster)))^2
      
      Sstar.t[[s.idx]] = cbind(time=lambda.t,est=c(test1[[3]]),se=sqrt(var.Sstar.final[[s.idx]]),stratum=s)
      #Sstar[[s.idx]] = test1[[4]]
      Zpred[[s.idx]] = cbind(time=lambda.t,est=c(Spred),se=c(sqrt(var.Spred.final[[s.idx]])),stratum=s)
      
      
      
    }
    
    ######################################### Adjusted survival end ################################
    
    
    
    ################################### SE(S_j - S_k) ##############################################
    names.strata <- sort(unique(strata))
    diff_var <- matrix(0,nrow=length(lambda.t),ncol=choose(length(names.strata),2) )
    jjj = 0
    for(i in 1: (length(names.strata) - 1)){
      temp.cluster.i = cluster[strata==names.strata[i]]
      for(k in (i+1):length(names.strata)){
        jjj = jjj + 1
        temp.cluster.k = cluster[strata==names.strata[k]]
        manoj2=0;manoj3=0;
        for(j in 1 : length(unique(cluster)) ){
          manoj2 = manoj2 + ( colSums(matrix(W1Sstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE) ) / length(unique(temp.cluster.i))  +
                                length(unique(temp.cluster.i))/length(unique(cluster))*
                                colSums(matrix(W3Sstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE)) / length(unique(temp.cluster.i))
                              
                              - colSums(matrix(W1Sstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t ),byrow=FALSE )  ) / length(unique(temp.cluster.k))
                              - length(unique(temp.cluster.k))/length(unique(cluster))*
                                colSums(matrix(W3Sstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t),byrow=FALSE)) / length(unique(temp.cluster.k))
          )^2 
        }
        
        diff_var[,jjj] = manoj2
      }
      
      
    }
    
    
    ################################### SE(S_j - S_k) #############################################
    
    
  }else{
    ###################################### SE of lambda0  #####################################
    
    Lambdas0 <- list()
    s.idx = 0
    var.lambda.final <- list()
    lambda.final <- list()
    var.Sstar.final <- list()
    var.Spred.final <- list()
    lambda.final <- list()
    Sstar.t <- list()
    Sstar <- list()
    
    s=1 #Because of unstratified model
    #for(trt.group in 1: number.of.stratum){
    s.idx =  1
    idx = which(strata==s)
    newd = d[idx,]
    temp.cluster = cluster[idx]
    
    idx.tlambda = NULL
    for(i in 1 : length(lambda.t)){
      if(lambda.t[i]>=min(newd$times)){
        idx.tlambda[i]=max(which(newd$times<=lambda.t[i]))
      }
      else{
        idx.tlambda[i]=1
      }
    }
    
    
    test = se_lambda_strata_COX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
                                length(unique(temp.cluster)),solve(I.actual),idx.tlambda,nrow(d),etas,length(unique(cluster)))
    Lambdas0[[s.idx]] <- cbind(time=newd$times,H0=cumsum(c(test[[2]])))
    
    W1 <- test[[3]]
    W3 <- t(test[[4]])
    
    #W1Sstar <- test[[6]]
    #W3Sstar <- t(test[[7]])
    
    manoj = 0; manoj1=0;
    for(j in 1 : length(unique(cluster))){
      
      manoj = manoj + (colSums(matrix(W1[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                         length(unique(temp.cluster))/length(unique(cluster))*
                         colSums(matrix(W3[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
      
      ## manoj1 = manoj1 + (colSums(matrix(W1Sstar[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
      ##                      length(unique(temp.cluster))/length(unique(cluster))*
      ##                      colSums(matrix(W3Sstar[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
      
    }
    
    var.lambda.final[[s.idx]] = manoj/(length(unique(temp.cluster)))^2
    #var.Sstar.final[[s.idx]] = manoj1/(length(unique(temp.cluster)))^2
    
    lambda.final[[s.idx]] =cbind(time=lambda.t,est=c(test[[5]]),se=sqrt(var.lambda.final[[s.idx]]))
    #Sstar.t[[s.idx]] = cbind(time=lambda.t,est=c(test[[8]]),var=var.Sstar.final[[s.idx]])
    #Sstar[[s.idx]] = cbind(time=newd$times,Star=c(test[[9]]))
    
    #}
    
    
    
    ###################################### SE of lambda0 end  #####################################
    
    ######################################### Adjusted survival #####################################
    s.idx = 0
    W1Sstar <- list()
    W3Sstar <- list()
    
    W3pred <- list(); W3predfirst <- list(); Spred <- list(); Zpred <- list();
    
    adjusted.covs = covs
    g <- matrix(0,number.of.stratum,(number.of.stratum-1))
    for(i in 2 : number.of.stratum){g[i,(i-1)]=1}
    s = 1 #Unstratified Model
    for(trt.group in 1 : number.of.stratum){
      newZ0 = c(g[trt.group,],Z0) #For S(t|Z0)
      if(ncol(g)==1){
        if(trt.group==1){
          adjusted.covs[,1:(number.of.stratum-1)] = 0
        }else{
          adjusted.covs[,1:(number.of.stratum-1)] = 1
        }
      }else{
        adjusted.covs[,1:(number.of.stratum-1)] = t(replicate(nrow(adjusted.covs),g[trt.group,]))
      }
      
      s.idx = s.idx + 1
      idx = which(strata==s)
      newd = d[idx,]
      temp.cluster = cluster[idx]
      
      idx.tlambda = NULL
      for(i in 1 : length(lambda.t)){
        if(lambda.t[i]>=min(newd$times)){
          idx.tlambda[i]=max(which(newd$times<=lambda.t[i]))
        }
        else{
          idx.tlambda[i]=1
        }
      }
      test1 = se_adjusted_strata_COX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
                                     length(unique(temp.cluster)),solve(I.actual),idx.tlambda,nrow(d),etas,length(unique(cluster)),c(lambda.final[[1]][,2]),
                                     data.matrix(adjusted.covs),b,newZ0 )
      ## test1 = se_adjusted_strata_COX(newd$times,newd$causes,data.matrix(newd[,-c(1:4)]),nrow(newd),ncol(covs),b,
      ##                   nrow(newd),solve(I.actual),idx.tlambda,nrow(d),etas,nrow(d),c(lambda.final[[s.idx]]),
      ##                  data.matrix(covs) )
      W1Sstar[[s.idx]] <- test1[[1]]
      W3Sstar[[s.idx]] <- t(test1[[2]])
      
      W3pred[[s.idx]] <- t(test1[[5]]); W3predfirst[[s.idx]] <- test1[[6]]; Spred[[s.idx]] <- test1[[4]];
      
      manoj1=0;manojpred=0;
      for(j in 1 : length(unique(cluster))){
        manoj1 = manoj1 + (colSums(matrix(W1Sstar[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                             length(unique(temp.cluster))/length(unique(cluster))*
                             colSums(matrix(W3Sstar[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        manojpred = manojpred + (colSums(matrix(W1[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                                   length(unique(temp.cluster))/length(unique(cluster))*
                                   colSums(matrix(W3pred[[s.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
      }
      
      
      var.Sstar.final[[s.idx]] = manoj1/(length(unique(temp.cluster)))^2
      var.Spred.final[[s.idx]] = manojpred * (W3predfirst[[s.idx]])^2/(length(unique(temp.cluster)))^2
      Sstar.t[[s.idx]] = cbind(time=lambda.t,est=c(test1[[3]]),se=sqrt(var.Sstar.final[[s.idx]]),stratum=as.numeric(stratum.names[trt.group]))
      #Sstar[[s.idx]] = test1[[4]]
      Zpred[[s.idx]] = cbind(time=lambda.t,est=c(Spred[[s.idx]]),se=c(sqrt(var.Spred.final[[s.idx]])),stratum=as.numeric(stratum.names[trt.group]))
      
      
      
    }
    
    ######################################### Adjusted survival end ################################
      
      ################################### SE(S_j - S_k) ##############################################
      #names.strata <- sort(unique(strata))
      diff_var <- matrix(0,nrow=length(lambda.t),ncol=choose(number.of.stratum,2) )
      jjj = 0
      for(i in 1: (number.of.stratum - 1)){
        #temp.cluster.i = cluster[strata==names.strata[i]]
        for(k in (i+1):number.of.stratum){
          jjj = jjj + 1
          #temp.cluster.k = cluster[strata==names.strata[k]]
          manoj2=0;manoj3=0;
          for(j in 1 : length(unique(cluster)) ){
            manoj2 = manoj2 + ( colSums(matrix(W1Sstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE) )   +
                                  colSums(matrix(W3Sstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) 
                                
                                - colSums(matrix(W1Sstar[[k]][cluster==j,],ncol=length(lambda.t ),byrow=FALSE )  ) 
                                - colSums(matrix(W3Sstar[[k]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) 
            )^2 
          }
          
          diff_var[,jjj] = manoj2/(length(unique(cluster)))^2
        }
        
        
      }
      
      
      ################################### SE(S_j - S_k) #############################################
      
    
  }
  
  ########################### Individual p-value ###############################################
  #Individual p-value
  ind.p.values <- 1 - (pchisq( (c(b)/sqrt(diag(var)) ) ^2,df=1 ) )
  names(ind.p.values) <- list.variable.names
  colnames(b) <- list.variable.names
  colnames(var) <- list.variable.names
  rownames(var) <-list.variable.names
  
  ########################### Individual p-value end ###############################################
  
  ################################# diff_var column names ##########################################
  jjj = 0
  title_diff_var = rep(0,choose(number.of.stratum,2))
  for(i in 1 :(number.of.stratum-1) ){
    if(number.of.stratum > 2){
      
      for(k in (i+1):number.of.stratum){
        jjj = jjj + 1
        #print(jjj)
        title_diff_var[jjj] = paste("SE_",stratum.names[i],stratum.names[k],sep="")
      }
    }else{
      title_diff_var = rep(0,(number.of.stratum-1))
      title_diff_var[1] = paste("SE_",stratum.names[1],stratum.names[2],sep="")
    }
    
  }
  
  colnames(diff_var)=title_diff_var
  ################################# diff_var column names end ########################################  
  
  # lambda.names = NULL; Sstar.names = NULL;
  # for(i in 1 : length(stratum.names)){
  #   lambda.names[i] = paste("Cum Base Hazard (Stratum ",stratum.names[i],")",sep="")
  #   Sstar.names[i] = paste("Adjusted SURV (Stratum ",stratum.names[i],")",sep="")
  # }
  # if(stratified.model==TRUE){
  #   names(lambda.final) = lambda.names
  #   names(Sstar.t) = Sstar.names
  # }else{
  #   names(lambda.final) = "Cum Base Hazard"
  #   names(Sstar.t) = Sstar.names
  # }
  
  
  return(list(coef=b,
              p.values = ind.p.values,
              var=var,
              #var = solve(I.actual),
              infor=I.actual,
              #score=U.actual,
              loglikelihood=loglik,
              n = nreturn,
              nevents = ereturn,
              nclusters = creturn,
              nstrata = sreturn,
              #all.lamda0.est=Lambdas0,
              CumBaseHaz.t = lambda.final,
              Spredict.t = Zpred,
              #var.lambda0.t = var.lambda.final,
              AdjustedS.t = Sstar.t,
              Adjusted.se.diff=sqrt(diff_var)
              ))
}else{
  return(list(coef=b,
              p.values = ind.p.values,
              var=var,
              #var = solve(I.actual),
              infor=I.actual,
              #score=U.actual,
              loglikelihood=loglik,
              n = nreturn,
              nevents = ereturn,
              nclusters = creturn,
              nstrata = sreturn
  ))
}
  
}

