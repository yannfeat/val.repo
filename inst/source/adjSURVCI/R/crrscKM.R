#' Stratified Proportional Subdistribution Hazards Model For Clustered Competing Risks Data With Covariate-Independent Censoring
#' @description Stratified proportional subdistribution hazards model for clustered competing risks data.
#' The survival probability of the censoring distribution is obtained using the Kaplan-Meier estimates.
#' The estimates of the cumulative baseline hazard along with their standard errors are provided at the
#' pre-specified time points.
#' Furthermore, the adjusted cumulative incidence rates along with their standard errors are calculated at pre-specified time points. The standard errors of the
#' the difference in adjusted cumulative incidence rates between the groups are also provided.
#' Finally, the estimated adjusted cumulative incidence rates given vector \code{Z0} along with their standard errors are provided at
#' pre-specified time points. Tied data are handled by adding a tiny random shift from a normal distribution with mean 0 and standard deviation
#' 1e-09.
#' @param times Failure/censored times.
#' @param causes Failure code for each failure type (1 or 2) and 0 for censoring.
#' @param covariates Matrix of covariates. Dummy variables must be created for categorical covariates.
#' @param treatment Treatment variable.
#' @param clusters Cluster variable. Independent data is assumed if this is not provided.
#' @param cencode Code for censoring. By default it is 0.
#' @param failcode Code for the failure type of interest. By default it is 1.
#' @param stratified.model \code{TRUE} or \code{FALSE}. By default, it is \code{TRUE} for stratified model. The stratification variable is \code{treatment}.
#' If this is \code{FALSE} and \code{est.t=TRUE}, then the \code{treatment} variable still needs to be provided and will be used as a covariate.
#' @param est.t \code{TRUE} or \code{FALSE}. By default this is \code{FALSE}. If it is \code{TRUE} then estimates of cumulative baseline hazard, adjusted cumulative incidence and predicted cumulative incidence are provided along with their standard errors at pre-specified time points.
#' @param pre.t Pre-specified time points.
#' By default these are all main event times.
#' @param Z0 Covariate vector for prediction. By default this vector is a zero vector.
#' 
#' @return Returns a list with the following components. If \code{est.t=FALSE} then only upto
#' $nstrata are provided.
#' \item{$coef}{Parameter estimates}
#' \item{$p.value}{p-value of regression coefficients}
#' \item{$var}{Covariance matrix of parameter estimates}
#' \item{$infor}{Information matrix}
#' \item{$loglikelihood}{Maximum log-likelihood value}
#' \item{$n}{Total number of observations used}
#' \item{$nevents}{Total number of events and censored observations}
#' \item{$nclusters}{Total number of clusters}
#' \item{$nstrata}{Total number of treatment groups}
#' \item{$CumBaseHaz.t}{Cumulative basline hazard estimates and their standard errors}
#' \item{$Fpredict.t}{Predicted cumulative incidence and their standard errors}
#' \item{$AdjustedF.t}{Adjusted cumulative incidence and their standard errors}
#' \item{$Adjusted.se.diff}{Standard error of the difference of adjusted cumulative incidence between the treatment groups}
#' @export
#' @examples
#' #Simulated data
#' alpha = 0.5
#' d = simulate_CR_data(n=4,m=50,alpha=alpha,beta1=c(0.7,-0.7,-0.5)*1/alpha,
#' beta2=c(0.5,-0.5,1),betaC=c(0,0,0)*1/alpha,lambdaC=0.59)
#' 
#' #Stratified Model with est.t=TRUE
#' model1 <- crrscKM(times=d[,1],causes=d[,2],covariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],stratified.model=TRUE,est.t=TRUE,
#' pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Unstratified Model with est.t=TRUE
#' model2 <- crrscKM(times=d[,1],causes=d[,2],covariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],stratified.model=FALSE,est.t=TRUE,
#' pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Stratified Model with est.t=FALSE
#' model3 <- crrscKM(times=d[,1],causes=d[,2],covariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],stratified.model=TRUE,est.t=FALSE,
#' pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Unstratified Model with est.t=FALSE.
#' #Create dummy variables first
#' dummy <- model.matrix(~ factor(d[,3]))[,-1]
#' model4 <- crrscKM(times=d[,1],causes=d[,2],covariates=cbind(d[,4:5],dummy),
#' clusters=d[,6],stratified.model=FALSE,est.t=FALSE,
#' pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Only continuous covariates are available.
#' model5 <- crrscKM(times=d[,1],causes=d[,2],covariates=d[,4:5],
#' clusters=d[,6],stratified.model=FALSE,est.t=FALSE,
#' pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))

crrscKM <- function(times,
                    causes,
                    covariates, 
                    treatment=NULL,
                    clusters=1:length(times),
                    cencode=0,
                    failcode=1,
                    stratified.model=TRUE,
                    est.t = FALSE,
                    pre.t=sort(times[causes==failcode]),
                    Z0=NULL){
  ################################### Housekeeping stuff ############################
  if(is.null(Z0)){Z0=rep(0,ncol(covariates))}
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
  
  lambda.t <- pre.t
  stratas <- treatment
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
  d = data.frame(times,causes,stratas,clusters,covariates)
  #Handling duplicate times
  id.duplicate = which(duplicated(d$times))
  d[id.duplicate,"times"] = d$times[id.duplicate] + rnorm(length(id.duplicate),0,0.000000001)
  
  d = d[order(d$times),] #Order the data by time
  d = na.omit(d) #Removing missing observations
  nreturn = nrow(d)
  ereturn = table(d$causes)
  ftime = d$times
  cenind = ifelse(d$causes==cencode,1,0)
  fstatus = ifelse(d$causes==failcode,1,2*(1-cenind))
  #ucl = sort(unique.default(d$clusters))
  ucl = unique.default(d$clusters)
  cluster = match(d$clusters,ucl)
  creturn = length(unique(cluster))
  # cov.names <- rep(0,ncol(covariates))
  # for(i in 1 : ncol(covariates)){
  #   if(ncol(covariates)==1){
  #     cov.names[i] <- "covs"
  #   }
  #   else{
  #     cov.names[i] <- paste0("covs.",i)
  #   }
  # }
  # covs <- d[,cov.names]
  
  #covs <- d[,colnames(covariates)]
  
  covs <- d[,-c(1:4)]
  
  
  if(is.vector(covs)){covs=matrix(covs)}

  
  strata = d$stratas
  sreturn = length(unique(strata))
  ntotal <- length(ftime)
  remove(times,causes,covariates,stratas,clusters)
  ################################### Housekeeping stuff end #########################
  
  ################################### Estimate survival prob of censoring and main cause ############
  b = rep(0,ncol(covs))
  delta=2
  Ufinal <- list()
  #while loop for NR starts here
  while(max(abs(delta)) > 10^(-8)){
    I.actual = matrix(0,ncol=ncol(covs),nrow=ncol(covs)); U.actual=matrix(0,nrow=ncol(covs),ncol=1); loglik=0;
    s.idx = 0
    Is = list()
    for(s in sort(unique(strata))){
      s.idx = s.idx + 1
      idd <- which(strata==s)
      temp.time = ftime[idd]
      #temp.surv = u$surv[aa:bb,idd]
      temp.cenind = cenind[idd]
      #Weight matrix for strata s
      # fit KM for censoring
      u = do.call('survfit',list(formula=Surv(temp.time,temp.cenind)~1,
                                 data=data.frame(temp.time,temp.cenind)))
      u = summary(u,times=sort(temp.time*(1-.Machine$double.eps)))
      uuu = u$surv
      Ufinal[[s.idx]]=uuu
      
      #jj <- jj +1
      #aa <- bb + 1
      #bb <- (aa-1) + pos[jj]
      
      temp.causes = fstatus[idd]
      temp.covs = data.matrix(covs[idd,])
      #temp.cencovs = cencovs[idd,]
      
      #Calling function written in C++
      outs = betaestKM(temp.time,temp.causes,temp.covs,length(temp.time),ncol(temp.covs),uuu,b)
      
      #Adding for each strata
      I.actual = I.actual + outs[[4]]
      U.actual = U.actual + outs[[3]]
      loglik = loglik + outs[[2]]
      Is[[s.idx]] = outs[[4]]
    }
    
    b = outs[[1]] + c(solve(I.actual,U.actual))
    delta = solve(I.actual,U.actual)
    
  }
    
  ################################### Estimate survival prob of censoring and main cause end ############
  
  ###################################### SE of beta1 #########################################
  etas <- matrix(0,nrow=ncol(covs),ncol=length(ftime))
  psis <- etas
  s.idx=0
  remove(temp.time,temp.causes,temp.covs)
  for(s in sort(unique(strata))){
    s.idx = s.idx + 1
    idd <- which(strata==s)
    temp.time = ftime[idd]
    temp.causes = fstatus[idd]
    temp.covs = data.matrix(covs[idd,])
    temp.cluster = cluster[idd]
    
    c=se_beta_strata_KM(temp.time,temp.causes,temp.covs,length(temp.time),ncol(temp.covs),
                        Ufinal[[s.idx]],b,length(unique(temp.cluster)))
 
    etas[,idd] <- c[[1]]
    psis[,idd] <- c[[2]]
    
  }
  
  #Variance of beta
  # middle = etas + psis
  # V= 0
  # for(j in sort(unique(cluster))){
  #   if(is.null(dim(middle[,cluster==j]))){
  #     temp = sum(middle[,cluster==j])
  #   }else if (dim(middle[,cluster==j])[2] > 1){
  #     temp = apply(middle[,cluster==j],1,sum ) 
  #   }
  #   #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
  #   V = V + temp %*% t(temp)
  # }
  
  middle = etas + psis
  V= 0
  if(is.null(cluster.check)){
    for(j in sort(unique(cluster))){
      #if(is.null(dim(middle[,cluster==j]))){
      temp = middle[,cluster==j]
      #}else{
      #temp = apply(middle[,cluster==j],1,sum ) 
      #}
      #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
      V = V + temp %*% t(temp)
    }
  }else{
    if(ncol(temp.covs)==1){
      for(j in sort(unique(cluster))){
        if(is.null(dim(middle[,cluster==j])) ){
          
          temp = sum(middle[,cluster==j])
        }
        # else{
        #   temp = apply(middle[,cluster==j],1,sum ) 
        # }
        #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
        V = V + temp %*% t(temp)
      }
    }else{
      for(j in sort(unique(cluster))){
        if(is.null(dim(middle[,cluster==j])) ){
          
          temp = middle[,cluster==j]
        }else{
          temp = apply(middle[,cluster==j],1,sum )
        }
        #temp = ifelse(is.null(dim(middle[,cluster==j])),middle[,cluster==j], apply(middle[,cluster==j],1,sum ) )
        V = V + temp %*% t(temp)
      }
    }
    
    
  }
  
  var = solve(I.actual) %*% (V) %*% solve(I.actual)
  ###################################### SE of beta1 end #####################################
  
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
    ###################################### Lambda0 estimate and SE ####################################
    s.idx=0
    var.lambda.final <- list()
    var.Fstar.final <- list()
    lambda.final <- list()
    all.Lambda <- list()
    Fstar <- list()
    Fstar.t <- list()
    W1Fstar <- list()
    W2Fstar <- list()
    W3Fstar <- list()
    var.Fpred.final <- list()
    
    Zpred <- list()
    for(s in sort(unique(strata))){
      
      s.idx = s.idx + 1
      idd <- which(strata==s)
      temp.time = ftime[idd]
      temp.causes = fstatus[idd]
      temp.covs = data.matrix(covs[idd,])
      temp.cluster = cluster[idd]
      
      idx.tlambda = NULL
      for(i in 1 : length(lambda.t)){
        if(lambda.t[i]>=min(temp.time)){
          idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
        }
        else{
          idx.tlambda[i]=1
        }
        
      }
      
      # idx.tlambda = NULL
      # for(i in 1 : length(lambda.t)){
      #   idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
      # 
      # }
      
      cc=se_beta_lambda_strata_KM_diff(temp.time,temp.causes,temp.covs,length(temp.time),
                                       length(ftime),ncol(temp.covs),
                                       Ufinal[[s.idx]],b,temp.cluster,cluster,length(unique(temp.cluster)),
                                       length(unique(cluster)),solve(I.actual),
                                       as.integer(idx.tlambda),(etas+psis),nrow(d),data.matrix(covs),Zpred=Z0)
      W1 <- cc[[1]]
      W2 <- cc[[2]]
      W3 <- t(cc[[3]])
      W3pred <- t(cc[[12]]); W3predfirst <- cc[[13]]; Fpred <- cc[[11]];
      
      W1Fstar[[s.idx]] <- cc[[7]]
      W2Fstar[[s.idx]] <- cc[[8]]
      W3Fstar[[s.idx]] <- t(cc[[9]])
      
      # manoj = 0  
      # for(j in 1 : length(unique(cluster))){
      #   manoj = manoj + (colSums(W1[temp.cluster==j,]) + 
      #                      colSums(W2[temp.cluster==j,]) +
      #                      length(unique(temp.cluster))/length(unique(cluster))*
      #                      colSums(W3[cluster==j,]))^2
      # }
      
      manoj = 0;manoj1=0; manojpred=0;  
      for(j in 1 : length(unique(cluster))){
        
        manoj = manoj + (colSums(matrix(W1[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                           colSums(matrix(W2[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                           length(unique(temp.cluster))/length(unique(cluster))*
                           colSums(matrix(W3[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        manoj1 = manoj1 + (colSums(matrix(W1Fstar[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                             colSums(matrix(W2Fstar[[s.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                             length(unique(temp.cluster))/length(unique(cluster))*
                             colSums(matrix(W3Fstar[[s.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        manojpred = manojpred + (colSums(matrix(W1[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                                   colSums(matrix(W2[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                                   length(unique(temp.cluster))/length(unique(cluster))*
                                   colSums(matrix(W3pred[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
      }
      
      var.lambda.final[[s.idx]] = manoj/(length(unique(temp.cluster)))^2
      var.Fstar.final[[s.idx]] = manoj1/(length(unique(temp.cluster)))^2
      var.Fpred.final[[s.idx]] = manojpred * (W3predfirst)^2/(length(unique(temp.cluster)))^2
      
      lambda.final[[s.idx]] = cbind(time=lambda.t,est=c(cc[[4]]),se=sqrt(var.lambda.final[[s.idx]]),stratum=s)
      #all.Lambda[[s.idx]] = cbind(time=temp.time,CumBaseHaz=(c(cc[[5]])))
      #Fstar[[s.idx]] = cbind(time=temp.time,Fstar=c(cc[[6]]))
      Fstar.t[[s.idx]] = cbind(time=lambda.t,est=c(cc[[10]]),se=sqrt(var.Fstar.final[[s.idx]]),stratum=s)
      Zpred[[s.idx]] = cbind(time=lambda.t,est=c(Fpred),se=c(sqrt(var.Fpred.final[[s.idx]])),stratum=s)
    }
    ###################################### Lambda0 estimate and SE end #################################
    
    ################################### SE(S_j - S_k) ##############################################
    names.strata <- sort(unique(strata))
    #if(length(names.strata)>1){
    diff_var <- matrix(0,nrow=length(lambda.t),ncol=choose(length(names.strata),2) )
    jjj = 0
    for(i in 1: (length(names.strata) - 1)){
      temp.cluster.i = cluster[strata==names.strata[i]]
      for(k in (i+1):length(names.strata)){
        jjj = jjj + 1
        temp.cluster.k = cluster[strata==names.strata[k]]
        manoj2=0;manoj3=0;
        for(j in 1 : length(unique(cluster)) ){
          manoj2 = manoj2 + ( colSums(matrix(W1Fstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE) ) / length(unique(temp.cluster.i))  +
                                length(unique(temp.cluster.i))/length(unique(cluster))*
                                colSums(matrix(W3Fstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE)) / length(unique(temp.cluster.i)) +
                                colSums(matrix(W2Fstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE) ) / length(unique(temp.cluster.i))
                              
                              - colSums(matrix(W1Fstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t ),byrow=FALSE )  ) / length(unique(temp.cluster.k))
                              - length(unique(temp.cluster.k))/length(unique(cluster))*
                                colSums(matrix(W3Fstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t),byrow=FALSE)) / length(unique(temp.cluster.k)) -
                                colSums(matrix(W2Fstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t ),byrow=FALSE )  ) / length(unique(temp.cluster.k))
          )^2 
        }
        
        diff_var[,jjj] = manoj2
      }
      
      
    }

    #}
    ################################### SE(S_j - S_k) #############################################
  }else{
    ###################################### Lambda0 estimate and SE ####################################
    s.idx=0
    var.lambda.final <- list()
    var.Fstar.final <- list()
    var.Fpred.final <- list()
    lambda.final <- list()
    all.Lambda <- list()
    Fstar <- list()
    Fstar.t <- list()
    W1Fstar <- list()
    W2Fstar <- list()
    W3Fstar <- list()
    
    W3pred <- list(); W3predfirst <- list(); Fpred <- list(); Zpred <- list();
    
    adjusted.covs = covs
    g <- matrix(0,number.of.stratum,(number.of.stratum-1))
    for(i in 2 : number.of.stratum){g[i,(i-1)]=1}
    
    s = 1 #for unstratified model
    trt.group.idx = 0
    for(trt.group in 1:number.of.stratum){
      newZ0 = c(g[trt.group,],Z0) #For F(t|Z)
      if(ncol(g)==1){
        if(trt.group==1){
          adjusted.covs[,1:(number.of.stratum-1)] = 0
        }else{
          adjusted.covs[,1:(number.of.stratum-1)] = 1
        }
      }else{
        adjusted.covs[,1:(number.of.stratum-1)] = t(replicate(nrow(adjusted.covs),g[trt.group,]))
      }
      
      
      s.idx = 1
      trt.group.idx = trt.group.idx + 1
      
      idd <- which(strata==s)
      temp.time = ftime[idd]
      temp.causes = fstatus[idd]
      temp.covs = data.matrix(covs[idd,])
      temp.cluster = cluster[idd]
      
      idx.tlambda = NULL
      for(i in 1 : length(lambda.t)){
        if(lambda.t[i]>=min(temp.time)){
          idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
        }
        else{
          idx.tlambda[i]=1
        }
        
      }
      
      # idx.tlambda = NULL
      # for(i in 1 : length(lambda.t)){
      #   idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
      # 
      # }
      
      cc=se_beta_lambda_strata_KM_diff_unstratified(temp.time,temp.causes,temp.covs,length(temp.time),
                                                    length(ftime),ncol(temp.covs),
                                                    Ufinal[[s.idx]],b,temp.cluster,cluster,length(unique(temp.cluster)),
                                                    length(unique(cluster)),solve(I.actual),
                                                    as.integer(idx.tlambda),(etas+psis),nrow(d),data.matrix(adjusted.covs),b,Zpred=newZ0)
      W1 <- cc[[1]]
      W2 <- cc[[2]]
      W3 <- t(cc[[3]])
      
      W3pred[[trt.group.idx]] <- t(cc[[12]]); W3predfirst[[trt.group.idx]] <- cc[[13]]; Fpred[[trt.group.idx]] <- cc[[11]];
      
      W1Fstar[[trt.group.idx]] <- cc[[7]]
      W2Fstar[[trt.group.idx]] <- cc[[8]]
      W3Fstar[[trt.group.idx]] <- t(cc[[9]])
      
      # manoj = 0  
      # for(j in 1 : length(unique(cluster))){
      #   manoj = manoj + (colSums(W1[temp.cluster==j,]) + 
      #                      colSums(W2[temp.cluster==j,]) +
      #                      length(unique(temp.cluster))/length(unique(cluster))*
      #                      colSums(W3[cluster==j,]))^2
      # }
      
      manoj = 0;manoj1=0; manojpred=0; 
      for(j in 1 : length(unique(cluster))){
        
        manoj = manoj + (colSums(matrix(W1[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                           colSums(matrix(W2[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                           length(unique(temp.cluster))/length(unique(cluster))*
                           colSums(matrix(W3[cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        manoj1 = manoj1 + (colSums(matrix(W1Fstar[[trt.group.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                             colSums(matrix(W2Fstar[[trt.group.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                             length(unique(temp.cluster))/length(unique(cluster))*
                             colSums(matrix(W3Fstar[[trt.group.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        manojpred = manojpred + (colSums(matrix(W1[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                                   colSums(matrix(W2[temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                                   length(unique(temp.cluster))/length(unique(cluster))*
                                   colSums(matrix(W3pred[[trt.group.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
        
        
        
      }
      
      var.lambda.final[[s.idx]] = manoj/(length(unique(temp.cluster)))^2
      var.Fstar.final[[trt.group.idx]] = manoj1/(length(unique(temp.cluster)))^2
      var.Fpred.final[[trt.group.idx]] = manojpred * (W3predfirst[[trt.group.idx]])^2/(length(unique(temp.cluster)))^2
      
      lambda.final[[s.idx]] = cbind(time=lambda.t,est=c(cc[[4]]),se=sqrt(var.lambda.final[[s.idx]]))
      #all.Lambda[[s.idx]] = cbind(time=temp.time,CumBaseHaz=(c(cc[[5]])))
      #Fstar[[s.idx]] = cbind(time=temp.time,Fstar=c(cc[[6]]))
      Fstar.t[[trt.group.idx]] = cbind(time=lambda.t,est=c(cc[[10]]),se=sqrt(var.Fstar.final[[trt.group.idx]]),stratum=as.numeric(stratum.names[trt.group]))
      Zpred[[trt.group.idx]] = cbind(time=lambda.t,est=c(Fpred[[trt.group.idx]]),se=c(sqrt(var.Fpred.final[[trt.group.idx]])),
                                     stratum=as.numeric(stratum.names[trt.group]))
    }
    ###################################### Lambda0 estimate and SE end #################################
    
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
          manoj2 = manoj2 + ( colSums(matrix(W1Fstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE) )  +
                                colSums(matrix(W3Fstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE))  +
                                colSums(matrix(W2Fstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) 
                              
                              - colSums(matrix(W1Fstar[[k]][cluster==j,],ncol=length(lambda.t ),byrow=FALSE )  ) 
                              - colSums(matrix(W3Fstar[[k]][cluster==j,],ncol=length(lambda.t),byrow=FALSE))   -
                                colSums(matrix(W2Fstar[[k]][cluster==j,],ncol=length(lambda.t ),byrow=FALSE )  ) 
          )^2 
        }
        
        diff_var[,jjj] = manoj2/(length(unique(cluster)))^2
        #diff_var[,jjj] = manoj2
      }
      
      
    }
    
    #}
    ################################### SE(S_j - S_k) #############################################
  }
  

  
  # return(list(coef=b,var=var,lambda0.est=lambda.final,var.lambda0.est=var.lambda.final,all.lamda0.est=all.Lambda,Fstar.t=cc[[10]],
  #             var.Fstar.est = var.Fstar.final,Fstar.all=Fstar))
  #return(list(coef=b,var=var,lambda0.est=lambda.final,var.lambda0.est=var.lambda.final,all.lamda0.est=all.Lambda))

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

  return(list(coef=b,p.value=ind.p.values,
              var=var,
              infor = I.actual,
              loglikelihood = loglik,
              n = nreturn,
              nevents = ereturn,
              nclusters = creturn,
              nstrata = sreturn,
              CumBaseHaz.t=lambda.final,
              Fpredict.t = Zpred,
              #all.lamda0.est=all.Lambda,
              AdjustedF.t=Fstar.t,
              #Fstar.all=Fstar
              Adjusted.se.diff=sqrt(diff_var)
  ))
}else{
  return(list(coef=b,p.value=ind.p.values,
              var=var,
              infor = I.actual,
              loglikelihood = loglik,
              n = nreturn,
              nevents = ereturn,
              nclusters = creturn,
              nstrata = sreturn
  ))
}

  
}

