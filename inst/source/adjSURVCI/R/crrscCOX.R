#' Stratified Competing Proportional Subdistribution Hazards Model For Clustered Competing Risks Data With Covariate-Dependent Censoring
#' @description Stratified proportional subdistribution hazards model for clustered competing risks data. 
#' The stratified Cox proportional hazards model is fitted
#' for the censoring distribution. 
#' The estimates of the cumulative baseline hazard along with their standard errors are provided at the
#' pre-specified time points.
#' Furthermore, the adjusted cumulative incidence rates along with their standard errors are calculated at pre-specified time points. The standard error of the
#' the difference in adjusted cumulative incidence between the groups are also provided.
#' Finally, the estimates of adjusted cumulative incidence rates given vector \code{Z0} along with their standard errors are provided at
#' pre-specified time points. Tied data are handled by adding a tiny random shift from a normal distribution with mean 0 and standard deviation
#' 1e-09.
#' @param times Failure/censored times.
#' @param causes Failure code for each failure type (1 or 2) and 0 for censoring.
#' @param covariates Matrix of covariates. Dummy variables must be created for categorical covariates.
#' @param cencovariates Matrix of covariates for censoring. Dummy variable must be created
#' for categorical covariates.
#' @param treatment Treatment variable.
#' @param clusters Cluster variable. Independent data is assumed if this is not provided.
#' @param cencode Code for censoring. By default it is 0.
#' @param failcode Code for the failure type of interest. By default it is 1.
#' @param treatmentC Treatment variable for censoring. Could also be stratification variable.
#' @param stratified.model \code{TRUE} or \code{FALSE}. By default, it is \code{TRUE} for stratified model. The stratification variable is \code{treatment}.
#' If this is \code{FALSE} and \code{est.t=TRUE}, then the \code{treatment} variable still needs to be provided and will be used as a covariate.
#' @param stratified.model.cens \code{TRUE} or \code{FALSE}. By default, it is \code{TRUE} for stratified model for censoring. For unstratified model \code{treatmentC} does not need to be provided.
#' @param est.t \code{TRUE} or \code{FALSE}. By default this is \code{FALSE}. If it is \code{TRUE} then estimates of cumulative baseline hazard, adjusted cumulative incidence and predicted cumulative incidence are provided along with their standard errors at pre-specified time points.
#' @param pre.t Pre-specified time points.
#' By default these are all main event times.
#' @param Z0 Covariate vector for prediction. By default this vector is a zero vector.
#' 
#' @return Returns a list with the following components. If \code{est.t=FALSE} then only upto
#' $nstrataC are provided.
#' \item{$coef}{Parameter estimates}
#' \item{$p.value}{p-value of regression coefficients}
#' \item{$var}{Covariance matrix of parameter estimates}
#' \item{$infor}{Information matrix}
#' \item{$loglikelihood}{Maximum log-likelihood value}
#' \item{$n}{Total number of observations used}
#' \item{$nevents}{Total number of events and censored observations}
#' \item{$nclusters}{Total number of clusters}
#' \item{$nstrata}{Total number of treatment groups}
#' \item{$nstrataC}{Total number of treatment groups for censoring}
#' \item{$CumBaseHaz.t}{Cumulative basline hazard estimates and their standard errors}
#' \item{$Fpredict.t}{Predicted cumulative incidence and their standard errors}
#' \item{$AdjustedF.t}{Adjusted cumulative incidence and their standard errors}
#' \item{$Adjusted.se.diff}{Standard error of the difference of adjusted cumulative incidence between the treatment groups}
#' @export
#' @examples

#' #Simulated data
#' alpha = 0.5
#' d = simulate_CR_data(n=4,m=50,alpha=alpha,beta1=c(0.7,-0.7,-0.5)*1/alpha,
#' beta2=c(0.5,-0.5,1),betaC=c(2,-2,1)*1/alpha,lambdaC=0.59)
#' 
#' #Note: Since est.t=TRUE, model1 through model4 below will also output the 
#' #estimates of cumulative baseline hazard, adjusted probabilities and predicted 
#' #probabilities along with their standard errors.
#' 
#' #Stratified Model for the main cause and stratified model for censoring
#' model1 <- crrscCOX(times=d[,1],causes=d[,2],covariates=d[,4:5],cencovariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],treatmentC=d[,3],stratified.model=TRUE,
#' est.t=TRUE,stratified.model.cens=TRUE,pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Unstratified Model for the main cause and stratified model for censoring
#' model2 <- crrscCOX(times=d[,1],causes=d[,2],covariates=d[,4:5],cencovariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],treatmentC=d[,3],stratified.model=FALSE,
#' stratified.model.cens=TRUE,est.t=TRUE,pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Stratified Model for the main cause and unstratified model for censoring
#' model3 <- crrscCOX(times=d[,1],causes=d[,2],covariates=d[,4:5],cencovariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],stratified.model=TRUE,
#' est.t=TRUE,stratified.model.cens=FALSE,pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Unstratified Model for the main cause and unstratified model for censoring
#' model4 <- crrscCOX(times=d[,1],causes=d[,2],covariates=d[,4:5],cencovariates=d[,4:5],
#' treatment=d[,3],clusters=d[,6],stratified.model=FALSE,
#' stratified.model.cens=FALSE,est.t=TRUE,pre.t=sort(d$time[d$cause==1]),Z0=c(0.5,0.5))
#' 
#' #Now set est.t=FALSE which means the cumulative baseline hazard estimate, adjusted
#' #probabilities and predicted cumulative incidence are not returned.
#' 
#' #Assume only continuous covariates are available for main cause and censoring.
#' #In this case both stratified.model and stratified.model.cens need to be FALSE.
#' model5 <- crrscCOX(times=d[,1],causes=d[,2],covariates=d[,4:5],cencovariates=d[,4:5],
#' clusters=d[,6],stratified.model=FALSE,stratified.model.cens=FALSE,est.t=FALSE)

crrscCOX <- function(times,
                     causes,
                     covariates,
                     cencovariates,
                     treatment=NULL,
                     clusters=1:length(times),
                     cencode=0,
                     failcode=1,
                     treatmentC=NULL,
                     stratified.model=TRUE,
                     stratified.model.cens=TRUE,
                     est.t = FALSE,
                     pre.t=sort(times[causes==failcode]),
                     Z0=NULL){
  
    ################################### Housekeeping stuff ############################
    if(is.null(Z0)){Z0=rep(0,ncol(covariates))}
    if(stratified.model.cens==TRUE){
      if(missing(treatmentC)){stop("Please provide treatmentC.")
      }
    } 
    if(stratified.model.cens==FALSE & length(treatmentC)>=1){
    stop("Please don't provide treatmentC when stratified.model.cens=FALSE.")}
  
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
    if(stratified.model.cens==TRUE){
      stratasC <- treatmentC
    }

    number.of.stratum <- length(unique(stratas))
    
    if(stratified.model==FALSE & est.t==TRUE){ #Unstratified Model for main cause
      stratas.dummy = model.matrix(~factor(stratas))[,-1]
      covariates=cbind(stratas.dummy,covariates)
      if(number.of.stratum==2){
        colnames(covariates)[1] <- paste("Stratum",sort(unique(stratas))[2],sep=" ")
      }
    }
    stratum.names = as.character(sort(unique(stratas)))
    
    # if(stratified.model.cens==FALSE){ #Unstratified Model for censoring
    #   stratasC.dummy = model.matrix(~factor(stratasC))[,-1]
    #   cencovariates = cbind(stratasC.dummy,cencovariates)
    # }
    
    
    if(is.null(clusters)){
      clusters = 1 : length(times)
      cluster.check <- NULL
    }else{
      cluster.check <- "a"
    }
    if(stratified.model==FALSE){
      stratas = rep(1,length(times))
    }
    if(stratified.model.cens==FALSE){
      stratasC = rep(1,length(times))
    }
    
    
    if(is.vector(covariates)){covariates=matrix(covariates)}
    list.variable.names <- colnames(covariates)
    if(is.vector(cencovariates)){cencovariates=matrix(cencovariates)}
    #d = data.frame(times,deltas,causes,covariates,cencovariates,
    #stratas,clusters)
    d = data.frame(times,causes,stratas,clusters,stratasC,covariates)
    
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
    ucl = sort(unique.default(d$clusters))
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
    
    covs <- d[,-c(1:5)]
    
    # cencov.names <- rep(0,ncol(cencovariates))
    # for(i in 1 : ncol(cencovariates)){
    #   if(ncol(cencovariates)==1){
    #     cencov.names[i] <- "cencovs"
    #   }
    #   else{
    #     cencov.names[i] <- paste0("cencovs.",i)
    #   }
    # }
    # cencovs <- d[,cencov.names]
    
    #cencovs <- covs
    dc = data.frame(times,cencovariates)
    
    dc = dc[order(dc$times),] #Order the data by time
    cencovs <- dc[,-1]
    
    if(is.vector(covs)){covs=matrix(covs)}
    if(is.vector(cencovs)){cencovs=matrix(cencovs)}
    
    strata = d$stratas
    sreturn = length(unique(strata))
    strataC=d$stratasC
    sreturnC = length(unique(strataC))
    ntotal <- length(ftime)
    remove(times,causes,covariates,cencovariates,stratas,clusters,stratasC)
    ################################### Housekeeping stuff end #########################
    
    
    ################################### CENSORING DISTRIBUTION ################################
    pc <- ncol(cencovs)
    beta.cens <- rep(0,pc) #Initial guess
    dC <- data.frame(time=ftime,cens=cenind,cencovs=cencovs)
    cd.covs <- dC[,-c(1,2)]
    colnames(cd.covs)<- NULL
    #fit.cox <- coxph(Surv(time,cens)~data.matrix(cd.covs)+strata(strataC)+cluster(cluster),data=dC)
    #NR for beta estimation(censoring)
    delta=2 #just to get inside the while loop
    IC <- list()
    
    while(max(abs(delta)) > 10^(-8)){
      s.idxC <- 0
      I.actualC = matrix(0,ncol=pc,nrow=pc); U.actualC=matrix(0,nrow=pc,ncol=1); loglikC=0;
      for(i in sort(unique(strataC))){
        s.idxC = s.idxC + 1
        idx = which(strataC==i)
        newd = dC[idx,]
        rhostemp = rep(1,nrow(newd))
        mineC = mybetaestCENSORING(newd$time,newd$cens,as.matrix(newd[,-c(1,2)]),beta.cens,rhostemp)
        I.actualC = I.actualC + mineC[[4]]
        U.actualC = U.actualC + mineC[[3]]
        loglikC = loglikC + mineC[[2]]
        
        IC[[s.idxC]] = mineC[[4]]
        
      }
      
      beta.cens = mineC[[1]] + c(solve(I.actualC)%*%U.actualC)
      delta = c(solve(I.actualC)%*%U.actualC)
    }
    
    remove(newd,idx,rhostemp)
    
    
    #Lambda and survival estimate (censoring)	
    lambdasC <- list()
    LambdasC <- list()
    survsC <- list()
    j = 0
    for(i in sort(unique(strataC))){
      j = j + 1
      idx = which(strataC==i)
      newd = dC[idx,]
      rhostemp = rep(1,nrow(newd))
      minelambda = mylambdaestCENSORING(newd$time,newd$cens,as.matrix(newd[,-c(1,2)]),beta.cens,rhostemp)
      
      #Lambda estimate
      lambdasC[[j]] = minelambda[[1]]
      LambdasC[[j]] = cumsum(lambdasC[[j]])
      
      #Survival Probability
      surv.probC = matrix(NA,nrow=nrow(newd),ncol=nrow(newd))
      for(ii in 1 : nrow(newd)){
        surv.probC[ii,] = exp(-c(LambdasC[[j]]) * c(exp(beta.cens %*% t(as.matrix(newd[ii,-c(1,2)])))))
      }
      survsC[[j]] = surv.probC
    }
    
    ################################### CENSORING DISTRIBUTION END ############################
    
    ################################### ALIGNING SURV PROB WITH MAIN CAUSE IF STRATA FOR CENS IS DIFFERENT ############################
    newLambda0 <- rep(NA,length(strataC))
    mmm <- 0
    for(i in sort(unique(strataC))){
      idC <- which(strataC==i)
      mmm <- mmm + 1
      newLambda0[idC] <- LambdasC[[mmm]]
    }
    
    newSurvsC <- matrix(NA,length(strataC),length(strataC))
    for(i in 1 : length(strataC)){
      if(is.vector(cd.covs)){
        newSurvsC[i,] <- exp(-1*newLambda0 * c(exp((beta.cens)%*%t(as.matrix(cd.covs)[i,]))))
      }else{
        newSurvsC[i,] <- exp(-1*newLambda0 * c(exp((beta.cens)%*%t(as.matrix(cd.covs[i,])))))
      }
      
    }
    
    
    ################################### ALIGNING SURV PROB WITH MAIN CAUSE IF STRATA FOR CENS IS DIFFERENT END ########################
    
    
    ################################### Main cause optimization ################################
    b = rep(0,ncol(covs)) #Initial guess
    delta=2
    
    #while loop for NR starts here
    while(abs(max(delta)) > 10^(-9)){
      I.actual = matrix(0,ncol=ncol(covs),nrow=ncol(covs)); U.actual=matrix(0,nrow=ncol(covs),ncol=1); loglik=0;
      s.idx = 0
      Is = list()
      for(s in sort(unique(strata))){
        s.idx = s.idx + 1
        idd <- which(strata==s)
        temp.time = ftime[idd]
        temp.causes = fstatus[idd]
        temp.covs = data.matrix(covs[idd,])
        temp.cencovs = data.matrix(cencovs[idd,])
        
        #Calling function written in C++
        #outs = betaestCOX(temp.time,temp.causes,data.matrix(temp.covs),length(temp.time),ncol(temp.covs),survsC[[s.idx]],b)
        outs = betaestCOX(temp.time,temp.causes,(temp.covs),length(temp.time),ncol(temp.covs),newSurvsC[idd,idd],b)
        
        #Adding for each strata
        I.actual = I.actual + outs[[4]]
        U.actual = U.actual + outs[[3]]
        loglik = loglik + outs[[2]]
        Is[[s.idx]] = outs[[4]]
      }
      
      b = outs[[1]] + c(solve(I.actual,U.actual))
      delta = solve(I.actual,U.actual)
      
    }
    ################################### Main cause optimization end ############################
    
    ###################################### SE of beta1 #########################################
    s.idx <- 0
    etas <- matrix(NA,nrow=ncol(covs),ncol=length(ftime))
    psis <- etas
    for(s in sort(unique(strata))){
      s.idx = s.idx + 1
      idd <- which(strata==s)
      
      temp.time = ftime[idd]
      temp.causes = fstatus[idd]
      temp.covs = data.matrix(covs[idd,])
      temp.cencovs = data.matrix(cencovs[idd,])
      temp.cluster = cluster[idd]
      
      expg <- exp(c(temp.cencovs %*% c(beta.cens)))
      
      # c=se_beta_strata_COX(temp.time,temp.causes,data.matrix(temp.covs),data.matrix(temp.cencovs),length(temp.time),ncol(temp.covs),
      #                      survsC[[s.idx]],b,c(beta.cens),length(unique(temp.cluster)),
      #                      expg,solve(I.actualC) )
      c=se_beta_strata_COX(temp.time,temp.causes,temp.covs,temp.cencovs,length(temp.time),ncol(temp.covs),
                           newSurvsC[idd,idd],b,c(beta.cens),length(unique(temp.cluster)),
                           expg,solve(I.actualC) )
      
      etas[,idd] <- c[[1]] 
      
      #Because of second term we have to go through all stratum
      ########Added on 02/25/2021
      fix.obs <- length(temp.time)
      psi1 = c[[2]]
      psi2 = 0
      s.idx1 = 0
      for(ss in sort(unique(strata))){
        s.idx1 = s.idx1 + 1
        idd1 <- which(strata==ss)
        temp.time = ftime[idd1]
        temp.causes = fstatus[idd1]
        temp.covs = data.matrix(covs[idd1,])
        temp.cencovs = data.matrix(cencovs[idd1,])
        temp.cluster = cluster[idd1]
        expg <- exp(c(temp.cencovs %*% c(beta.cens)))
        
        
        # c1=se_beta_strata_COX1(temp.time,temp.causes,data.matrix(temp.covs),data.matrix(temp.cencovs),length(temp.time),
        #                        fix.obs,ncol(temp.covs),
        #                        survsC[[s.idx1]],b,c(beta.cens),length(unique(temp.cluster)),
        #                        expg,solve(I.actualC),c[[3]],length(unique(cluster)),c[[4]] )
        
        c1=se_beta_strata_COX1(temp.time,temp.causes,temp.covs,temp.cencovs,length(temp.time),
                               fix.obs,ncol(temp.covs),
                               newSurvsC[idd1,idd1],b,c(beta.cens),length(unique(temp.cluster)),
                               expg,solve(I.actualC),c[[3]],length(unique(cluster)),c[[4]] )
        psi2 = psi2 + c1[[1]]
      }
      
      psis[,idd] <- psi2 + psi1
      
      
      ########Added on 02/25/2021 end
      
    }
    
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
    # var.est <- data.frame(var.est)
    # colnames(var.est) <- list.variable.names
    # rownames(var.est) <- list.variable.names
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
      ###################################### Lambda0 estimate and SE #####################################
      s.idx2=0
      lambda.final <- list()
      var.lambda.final <- list()
      var.Fstar.final <- list()
      var.Fpred.final <- list()
      W1 <- list()
      W2 <- list()
      W3 <- list(); W3pred <- list(); W3predfirst <- list(); Fpred <- list(); Zpred <- list();
      W1Fstar <- list()
      W2Fstar <- list()
      W3Fstar <- list()
      Fstar <- list()
      Fstar.t <- list()
      all.Lambda <- list()
      
      for(sss in sort(unique(strata))){
        
        s.idx2 = s.idx2 + 1
        idd <- which(strata==sss)
        temp.time = ftime[idd]
        temp.causes = fstatus[idd]
        temp.covs = data.matrix(covs[idd,])
        temp.cencovs = data.matrix(cencovs[idd,])
        temp.cluster = cluster[idd]
        
        # idx.tlambda = NULL
        # for(i in 1 : length(lambda.t)){
        #   idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
        # 
        # }
        
        idx.tlambda = NULL
        for(i in 1 : length(lambda.t)){
          if(lambda.t[i]>=min(temp.time)){
            idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
          }
          else{
            idx.tlambda[i]=1
          }
          
        }
        
        expg <- exp(c(temp.cencovs %*% c(beta.cens)))
        
        # cc=se_beta_lambda_strata_COX_diff(temp.time,temp.causes,data.matrix(temp.covs),data.matrix(temp.cencovs),length(temp.time),
        #                                   ntotal,ncol(temp.covs),
        #                                   survsC[[s.idx2]],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
        #                                   length(unique(cluster)),expg,solve(I.actualC),
        #                                   solve(Reduce("+",Is)),
        #                                   as.integer(idx.tlambda),etas+psis)
        cc=se_beta_lambda_strata_COX_diff(temp.time,temp.causes,temp.covs,temp.cencovs,length(temp.time),
                                          ntotal,ncol(temp.covs),
                                          newSurvsC[idd,idd],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
                                          length(unique(cluster)),expg,solve(I.actualC),
                                          solve(Reduce("+",Is)),
                                          as.integer(idx.tlambda),etas+psis,nrow(d),data.matrix(covs),Zpred=Z0 )
        W1[[s.idx2]] <- cc[[1]]
        W2[[s.idx2]] <- cc[[2]]
        #W2[[s.idx2]] <- 0
        W3[[s.idx2]] <- t(cc[[3]]); W3pred[[s.idx2]] <- t(cc[[15]]); W3predfirst[[s.idx2]] <- cc[[16]]; Fpred[[s.idx2]] <- cc[[14]];
        
        W1Fstar[[s.idx2]] <- cc[[9]]
        W2Fstar[[s.idx2]] <- cc[[10]]
        #W2Fstar[[s.idx2]] <- 0
        W3Fstar[[s.idx2]] <- t(cc[[11]])
        
        lambda.final[[s.idx2]] = cc[[6]]
        
        
        #Need to go through all the stratum again due to the second term
        fix.obs <- length(temp.time)
        s.idx3 = 0
        for(sss1 in sort(unique(strata))){
          s.idx3 = s.idx3 + 1
          idd <- which(strata==sss1)
          temp.time = ftime[idd]
          
          temp.causes = fstatus[idd]
          temp.covs = data.matrix(covs[idd,])
          temp.cencovs = data.matrix(cencovs[idd,])
          temp.cluster = cluster[idd]
          
          # idx.tlambda = NULL
          # for(i in 1 : length(lambda.t)){
          #   idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
          # 
          # }
          
          idx.tlambda = NULL
          for(i in 1 : length(lambda.t)){
            if(lambda.t[i]>=min(temp.time)){
              idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
            }
            else{
              idx.tlambda[i]=1
            }
            
          }
          # 
          # expg <- exp(c(temp.cencovs %*% c(beta.cens)))
          
          # cc1=se_beta_lambda_strata_COX_diff1(temp.time,temp.causes,data.matrix(temp.covs),data.matrix(temp.cencovs),length(temp.time),
          #                                     fix.obs,ncol(temp.covs),
          #                                     survsC[[s.idx3]],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
          #                                     length(unique(cluster)),expg,solve(I.actualC),
          #                                     solve(Reduce("+",Is)),
          #                                     as.integer(idx.tlambda),etas+psis,cc[[4]],cc[[5]])
          
          cc1=se_beta_lambda_strata_COX_diff1(temp.time,temp.causes,temp.covs,temp.cencovs,length(temp.time),
                                              fix.obs,ncol(temp.covs),
                                              newSurvsC[idd,idd],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
                                              length(unique(cluster)),expg,solve(I.actualC),
                                              solve(Reduce("+",Is)),
                                              as.integer(idx.tlambda),etas+psis,cc[[4]],cc[[5]],nrow(d),data.matrix(covs))
          W2[[s.idx2]] = W2[[s.idx2]] + cc1[[1]]
          W2Fstar[[s.idx2]] = W2Fstar[[s.idx2]] + cc1[[2]]
        }
        
        idd <- which(strata==sss)
        temp.cluster = cluster[idd]
        
        manoj = 0; manoj1=0;  manojpred=0;
        for(j in 1 : length(unique(cluster))){
          
          manoj = manoj + (colSums(matrix(W1[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                             colSums(matrix(W2[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                             length(unique(temp.cluster))/length(unique(cluster))*
                             colSums(matrix(W3[[s.idx2]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
          
          manoj1 = manoj1 + (colSums(matrix(W1Fstar[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                               colSums(matrix(W2Fstar[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                               length(unique(temp.cluster))/length(unique(cluster))*
                               colSums(matrix(W3Fstar[[s.idx2]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
          
          manojpred = manojpred + (colSums(matrix(W1[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                                     colSums(matrix(W2[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                                     length(unique(temp.cluster))/length(unique(cluster))*
                                     colSums(matrix(W3pred[[s.idx2]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
          
        }
        
        var.lambda.final[[s.idx2]] = manoj/(length(unique(temp.cluster)))^2
        var.Fstar.final[[s.idx2]] = manoj1/(length(unique(temp.cluster)))^2
        var.Fpred.final[[s.idx2]] = manojpred * (W3predfirst[[s.idx2]])^2/(length(unique(temp.cluster)))^2
        
        lambda.final[[s.idx2]] = cbind(time=lambda.t,est=c(cc[[6]]),se=sqrt(var.lambda.final[[s.idx2]]),stratum=sss)
        #all.Lambda[[s.idx2]] = cbind(time=ftime[which(strata==sss)],CumBaseHaz=(c(cc[[7]])),stratum=sss)
        #Fstar[[s.idx2]] = cbind(time=ftime[which(strata==sss)],Fstar=c(cc[[8]]),stratum=sss)
        Fstar.t[[s.idx2]] = cbind(time=lambda.t,est=c(cc[[12]]),se=sqrt(var.Fstar.final[[s.idx2]]),stratum=sss)
        #lambda.final[[s.idx2]] = cc[[2]]
        Zpred[[s.idx2]] = cbind(time=lambda.t,est=c(Fpred[[s.idx2]]),se=c(sqrt(var.Fpred.final[[s.idx2]])),stratum=sss)
      }
      
      ###################################### Lambda0 estimate and SE end #################################
      
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
      
      ################################### SE(S_j - S_k) end #############################################
    }else{
      ###################################### Lambda0 estimate and SE #####################################
      s.idx2=0
      lambda.final <- list()
      var.lambda.final <- list()
      var.Fstar.final <- list()
      var.Fpred.final <- list()
      W1 <- list()
      W2 <- list()
      W3 <- list();W3pred <- list(); W3predfirst <- list(); Fpred <- list(); Zpred <- list();
      W1Fstar <- list()
      W2Fstar <- list()
      W3Fstar <- list()
      Fstar <- list()
      Fstar.t <- list()
      all.Lambda <- list()
      
      adjusted.covs = covs
      g <- matrix(0,number.of.stratum,(number.of.stratum-1))
      for(i in 2 : number.of.stratum){g[i,(i-1)]=1}
      sss=1 #Because of unstratified model
      trt.group.idx = 0
      #s.idx2 = 1
      for(trt.group in 1 : (number.of.stratum)){
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
        s.idx2 = 1
        trt.group.idx = trt.group.idx + 1
        idd <- which(strata==sss)
        temp.time = ftime[idd]
        temp.causes = fstatus[idd]
        temp.covs = data.matrix(covs[idd,])
        temp.cencovs = data.matrix(cencovs[idd,])
        temp.cluster = cluster[idd]
        #adjusted.covs[,trt.group]=1
        
        
        
        # idx.tlambda = NULL
        # for(i in 1 : length(lambda.t)){
        #   idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
        # 
        # }
        
        idx.tlambda = NULL
        for(i in 1 : length(lambda.t)){
          if(lambda.t[i]>=min(temp.time)){
            idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
          }
          else{
            idx.tlambda[i]=1
          }
          
        }
        
        expg <- exp(c(temp.cencovs %*% c(beta.cens)))
        
        # cc=se_beta_lambda_strata_COX_diff(temp.time,temp.causes,data.matrix(temp.covs),data.matrix(temp.cencovs),length(temp.time),
        #                                   ntotal,ncol(temp.covs),
        #                                   survsC[[s.idx2]],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
        #                                   length(unique(cluster)),expg,solve(I.actualC),
        #                                   solve(Reduce("+",Is)),
        #                                   as.integer(idx.tlambda),etas+psis)
        cc=se_beta_lambda_strata_COX_diff_unstratified(temp.time,temp.causes,temp.covs,temp.cencovs,length(temp.time),
                                                       ntotal,ncol(temp.covs),
                                                       newSurvsC[idd,idd],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
                                                       length(unique(cluster)),expg,solve(I.actualC),
                                                       solve(Reduce("+",Is)),
                                                       as.integer(idx.tlambda),etas+psis,nrow(d),data.matrix(adjusted.covs),
                                                       b,Zpred=newZ0)
        W1[[s.idx2]] <- cc[[1]]
        W2[[s.idx2]] <- cc[[2]]
        #W2[[s.idx2]] <- 0
        W3[[s.idx2]] <- t(cc[[3]]); W3pred[[trt.group.idx]] <- t(cc[[15]]); W3predfirst[[trt.group.idx]] <- cc[[16]]; Fpred[[trt.group.idx]] <- cc[[14]];
        
        W1Fstar[[trt.group.idx]] <- cc[[9]]
        W2Fstar[[trt.group.idx]] <- cc[[10]]
        #W2Fstar[[s.idx2]] <- 0
        W3Fstar[[trt.group.idx]] <- t(cc[[11]])
        
        lambda.final[[s.idx2]] = cc[[6]]
        
        
        # #Need to go through all the stratum again due to the second term
        # fix.obs <- length(temp.time)
        # s.idx3 = 0
        # #for(sss1 in sort(unique(strata))){
        # sss1 = 1
        # s.idx3 = s.idx3 + 1
        # idd <- which(strata==sss1)
        # temp.time = ftime[idd]
        # temp.causes = fstatus[idd]
        # temp.covs = data.matrix(covs[idd,])
        # temp.cencovs = data.matrix(cencovs[idd,])
        # temp.cluster = cluster[idd]
        # 
        # # idx.tlambda = NULL
        # # for(i in 1 : length(lambda.t)){
        # #   idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
        # # 
        # # }
        # 
        # # idx.tlambda = NULL
        # # for(i in 1 : length(lambda.t)){
        # #   if(lambda.t[i]>=min(temp.time)){
        # #     idx.tlambda[i]=max(which(temp.time<=lambda.t[i]))
        # #   }
        # #   else{
        # #     idx.tlambda[i]=1
        # #   }
        # #   
        # # }
        # # 
        # # expg <- exp(c(temp.cencovs %*% c(beta.cens)))
        # 
        # # cc1=se_beta_lambda_strata_COX_diff1(temp.time,temp.causes,data.matrix(temp.covs),data.matrix(temp.cencovs),length(temp.time),
        # #                                     fix.obs,ncol(temp.covs),
        # #                                     survsC[[s.idx3]],b,c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
        # #                                     length(unique(cluster)),expg,solve(I.actualC),
        # #                                     solve(Reduce("+",Is)),
        # #                                     as.integer(idx.tlambda),etas+psis,cc[[4]],cc[[5]])
        # 
        # cc1=se_beta_lambda_strata_COX_diff1_unstratified(temp.time,temp.causes,temp.covs,temp.cencovs,length(temp.time),
        #                                     fix.obs,ncol(temp.covs),
        #                                     newSurvsC[idd,idd],betas.for.adjusted[trt.group],c(beta.cens),temp.cluster,cluster,length(unique(temp.cluster)),
        #                                     length(unique(cluster)),expg,solve(I.actualC),
        #                                     solve(Reduce("+",Is)),
        #                                     as.integer(idx.tlambda),etas+psis,cc[[4]],cc[[5]],nrow(d),data.matrix(covs))
        # W2[[s.idx2]] = W2[[s.idx2]] + cc1[[1]]
        # W2Fstar[[trt.group.idx]] = W2Fstar[[trt.group.idx]] + cc1[[2]]
        
        
        #}
        
        idd <- which(strata==sss)
        temp.cluster = cluster[idd]
        
        manoj = 0; manoj1=0; manojpred=0;
        for(j in 1 : length(unique(cluster))){
          
          manoj = manoj + (colSums(matrix(W1[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                             colSums(matrix(W2[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                             length(unique(temp.cluster))/length(unique(cluster))*
                             colSums(matrix(W3[[s.idx2]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
          
          manoj1 = manoj1 + (colSums(matrix(W1Fstar[[trt.group.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                               colSums(matrix(W2Fstar[[trt.group.idx]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                               length(unique(temp.cluster))/length(unique(cluster))*
                               colSums(matrix(W3Fstar[[trt.group.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
          
          manojpred = manojpred + (colSums(matrix(W1[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) + 
                                     colSums(matrix(W2[[s.idx2]][temp.cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) +
                                     length(unique(temp.cluster))/length(unique(cluster))*
                                     colSums(matrix(W3pred[[trt.group.idx]][cluster==j,],ncol=length(lambda.t),byrow=FALSE)) )^2
          
        }
        
        var.lambda.final[[s.idx2]] = manoj/(length(unique(temp.cluster)))^2
        var.Fstar.final[[trt.group.idx]] = manoj1/(length(unique(temp.cluster)))^2
        var.Fpred.final[[trt.group.idx]] = manojpred * (W3predfirst[[trt.group.idx]])^2/(length(unique(temp.cluster)))^2
        
        lambda.final[[s.idx2]] = cbind(time=lambda.t,est=c(cc[[6]]),se=sqrt(var.lambda.final[[s.idx2]]))
        #all.Lambda[[s.idx2]] = cbind(time=ftime[which(strata==sss)],CumBaseHaz=(c(cc[[7]])))
        #Fstar[[s.idx2]] = cbind(time=ftime[which(strata==sss)],Fstar=c(cc[[8]]))
        Fstar.t[[trt.group.idx]] = cbind(time=lambda.t,est=c(cc[[12]]),se=sqrt(var.Fstar.final[[trt.group.idx]]),stratum=as.numeric(stratum.names[trt.group]))
        Zpred[[trt.group.idx]] = cbind(time=lambda.t,est=c(Fpred[[trt.group.idx]]),se=c(sqrt(var.Fpred.final[[trt.group.idx]])),
                                       stratum=as.numeric(stratum.names[trt.group]))
        #lambda.final[[s.idx2]] = cc[[2]]
        #}
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
            # manoj2 = manoj2 + ( colSums(matrix(W1Fstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE) ) / length(unique(temp.cluster.i))  +
            #                       length(unique(temp.cluster.i))/length(unique(cluster))*
            #                       colSums(matrix(W3Fstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE)) / length(unique(temp.cluster.i)) +
            #                       colSums(matrix(W2Fstar[[i]][temp.cluster.i==j,],ncol=length(lambda.t),byrow=FALSE) ) / length(unique(temp.cluster.i))
            #                     
            #                     - colSums(matrix(W1Fstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t ),byrow=FALSE )  ) / length(unique(temp.cluster.k))
            #                     - length(unique(temp.cluster.k))/length(unique(cluster))*
            #                       colSums(matrix(W3Fstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t),byrow=FALSE)) / length(unique(temp.cluster.k)) -
            #                       colSums(matrix(W2Fstar[[k]][temp.cluster.k==j,],ncol=length(lambda.t ),byrow=FALSE )  ) / length(unique(temp.cluster.k))
            # )^2 
            
            manoj2 = manoj2 + ( colSums(matrix(W1Fstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE) )  +
                                  colSums(matrix(W3Fstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE))  +
                                  colSums(matrix(W2Fstar[[i]][cluster==j,],ncol=length(lambda.t),byrow=FALSE) ) 
                                
                                -  colSums(matrix(W1Fstar[[k]][cluster==j,],ncol=length(lambda.t ),byrow=FALSE )  ) -
                                  colSums(matrix(W3Fstar[[k]][cluster==j,],ncol=length(lambda.t),byrow=FALSE))  -
                                  colSums(matrix(W2Fstar[[k]][cluster==j,],ncol=length(lambda.t ),byrow=FALSE )  ) 
            )^2 
          }
          
          diff_var[,jjj] = manoj2/(length(unique(cluster)))^2
        }
        
        
      }
      
      ################################### SE(S_j - S_k) end #############################################
    }
    
    
    #return(list(coef=b,var=var,lambda0.est=lambda.final,var.lambda0.est=var.lambda.final))
    
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
                nstrataC = sreturnC,
                CumBaseHaz.t=lambda.final,
                Fpredict.t = Zpred,
                #CumBaseHaz.all.t=all.Lambda,
                AdjustedF.t=Fstar.t,
                #AdjustedF.all.t=Fstar,
                Adjusted.se.diff=sqrt(diff_var)
    ))
  }else{
    return(list(coef=b,
                p.value=ind.p.values,
                var=var,
                infor = I.actual,
                loglikelihood = loglik,
                n = nreturn,
                nevents = ereturn,
                nclusters = creturn,
                nstrata = sreturn,
                nstrataC = sreturnC
                
    ))
  }

  
  
}


