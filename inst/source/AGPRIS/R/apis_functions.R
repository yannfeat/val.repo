

includeLag <- function(dataset,VAR,var.agg,lags=1){
 dataset0 <- dataset
 dataset0[['Anno']] <- dataset[['Anno']] + lags
 dataset <- plyr::join(dataset,dataset0[,c('Anno',var.agg,VAR)],
  by=c('Anno',var.agg),type='left',match='first') 
 names(dataset)[ncol(dataset)] <- paste(VAR,paste('_',lags,sep=''),sep='') 
 return(dataset) 
}



fdiff <- function(dataset,VAR,var.agg){
 datasetlag <- subset(dataset[,c(VAR,var.agg)],dataset$Anno<max(dataset$Anno))
 datasetlag$Anno <- datasetlag$Anno+1
 dataset <- plyr::join(dataset,datasetlag,by=var.agg)
 VARlag <- paste(VAR,'_1',sep='')
 names(dataset)[ncol(dataset)]=VARlag
 dataset <- subset(dataset,dataset$Anno > min(dataset$Anno))
 VARdiff <- paste('diff_',VAR,sep='')
 dataset[[VARdiff]] <- dataset[[VAR]] - dataset[[VARlag]]
 return(dataset[,c(var.agg,VAR,VARlag,VARdiff)])
} 
	



#' BCML estimator
#'
#' This function estimates a space time linear model according to the specified formula. It implements the BCML (or BCLSDV) estimator as in Elhorst (2010) \doi{10.1016/j.regsciurbeco.2010.03.003}.  
#' 
#' @param dataset STFDF with the data
#' @param yearStart First year considered in the estimation
#' @param yearEnd Last Anno considered in the estimation
#' @param var.agg Index of the spatial units
#' @param eq Formula to be estimated. It excludes the spatial lag
#' @param spatial Radius to define neighbors
#' @param estimation Either 'analytical' or 'numerical'. If 'analytical' is specified the concentrated maximum likelihood is estimated and then the other parameters are obtained analytically. Otherwise, all parameters are obtained through numerical maximization of the log-likelihood function.
#' @param corrBIAS Boolean. If TRUE, the bias correction is applied.
#' @param WMAT The spatial weight matrix
#'
#' @return A list with two objects. The first object is the estimates table. The second object is the log-likelihood evaluated at its maximum
#' @examples
#' \donttest{
#' library(maxLik)
#' library(matrixcalc)
#' 
#' set.seed(123)
#' sd = sim_data_fe(dataset=regsamp,N=100,TT=8,
#'                  spatial = 80,Tau = -0.2,Rho = 0.4,
#'                  Beta = 2,sdDev = 2,startingT = 10,
#'                  LONGLAT = TRUE)
#' est_bcml = bcml(dataset = sd[[1]],yearStart = 3,yearEnd = 9,
#'                 var.agg = 'Cod_Provincia',eq = Y~X1,
#'                   estimation = 'analytical',corrBIAS = TRUE,WMAT = sd[[2]])
#' est_bcml
#' }
#'
#' @export
bcml <- function(
 dataset,
 yearStart,
 yearEnd,
 var.agg='Cod_Provincia',
 eq,
 spatial = NULL,
 estimation='analytical',
 corrBIAS=TRUE,
 WMAT = NULL){


 
 if(class(dataset) %in% c('sp','STFDF')){ 
  dataset0 <- dataset@data
 }else{
  dataset0 <- dataset
 }
 Ninit <- length(levels(as.factor(dataset0[[var.agg]])))
 Tinit <- nrow(dataset0)/Ninit
 
 N <- length(levels(as.factor(dataset0[[var.agg]])))
 TT <- nrow(dataset0)/N

 iota <- rep(1,TT)
 Q <- kronecker(diag(TT) - (1/TT)*iota%*%t(iota),diag(N)) # this matrix is
 

 if(is.null(WMAT)){
 if(is.numeric(spatial) & !(spatial %in% c(0,'continuous','queen'))){
  dmax=spatial
  textDist = paste('dist',paste(dmax,'km'))
  dataset = sp::spTransform(dataset,sp::CRS("+proj=longlat"))
  netw2 = spdep::dnearneigh(sp::coordinates(dataset@sp),d1=0,d2=dmax,longlat=FALSE)
  matw0 = spdep::nb2mat(netw2,zero.policy=TRUE)
 }
  
 }else{
  matw0=WMAT
 }
 


 
 dataset0 <- dataset0[order(dataset0$Anno,
                           dataset0[[var.agg]],
						   decreasing=FALSE),]
 
 Yvar <- as.character(eq[[2]])
 sYvar <- paste('spat.',Yvar,sep='')
 dataset0[[sYvar]] =
  kronecker(diag(TT),matw0)%*%as.matrix(dataset0[,Yvar])

 Xvars <- strsplit(as.character(eq)[[3]],split='\\+')[[1]]
 Xvars <- gsub("\n    ","",Xvars)
 Xvars <- gsub(" ","",Xvars)
 
 EQvars <- c(Yvar,sYvar,Xvars)
 for(j in EQvars){
  dataset0[[paste('dm.',j,sep='')]] = 
   Q %*% dataset0[,j]
 }
 EQvarsQ <- paste('dm.',EQvars,sep='') 
 EQvarsALL <- c(EQvars,EQvarsQ)
 for(i in EQvarsALL){
  dataset0 <- includeLag(dataset=dataset0,
                        VAR=i,
                        var.agg=var.agg)						
 }                        
 EQvarsALL1 <- paste(EQvarsALL,'_1',sep='')
 dataset0 <- subset(dataset0,dataset0$Anno>=yearStart & dataset0$Anno<=yearEnd)
 
 
 
 N <- length(levels(as.factor(dataset0[[var.agg]])))
 TT <- nrow(dataset0)/N

 iota <- rep(1,TT)
 Q <- kronecker(diag(TT) - (1/TT)*iota%*%t(iota),diag(N)) # this matrix is
 
 
 Xstar <- as.matrix(dataset0[,paste('dm.',Xvars,sep='')])
 colnames(Xstar) <- paste('dm.',Xvars,sep='')
 
 Y_1star <- Q%*%as.matrix(dataset0[,paste(Yvar,'_1',sep='')])
 X_1star <- Q%*%as.matrix(dataset0[,paste(Xvars,'_1',sep='')])

 colnames(Y_1star) = paste('dm.',paste(Yvar,'_1',sep=''),sep='')
  colnames(X_1star) = paste('dm.',paste(Xvars,'_1',sep=''),sep='')

 XXstar <- as.matrix(cbind(Y_1star,Xstar))
 XX_1star <- as.matrix(cbind(Y_1star,X_1star))
 colnames(XXstar) <- c(colnames(Y_1star),colnames(Xstar))
 colnames(XX_1star) <- c(colnames(Y_1star),colnames(X_1star))

 Ystar0 <- as.matrix(dataset0[,paste('dm.',Yvar,sep='')])
 colnames(Ystar0) <- paste('dm.',Yvar,sep='')
 Ystar1 <- as.matrix(dataset0[,paste('dm.spat.',Yvar,sep='')])
 colnames(Ystar1) <- paste('dm.spat.',Yvar,sep='')
 
 par0 <- solve(t(XXstar)%*%XXstar)%*%t(XXstar)%*%Ystar0
 names(par0) <- rownames(par0)
 mod0 <- XXstar%*%par0
 e0 <- Ystar0-mod0
 
 par1 <- solve(t(XXstar)%*%XXstar)%*%t(XXstar)%*%Ystar1
 names(par1) <- rownames(par1)
 mod1 = XXstar%*%par1
 e1 <- Ystar1-mod1
 
  llrho <- function(rho){
   ll <- -((N*TT)/2)*log(t(e0 - rho*e1)%*%(e0-rho*e1)) +
   TT*log(det(diag(N)-rho*matw0))
   return(ll)
  }
 
  LL <- function(pa){
   -((N*TT)/2)*log(2*pi*pa[1]) +
   TT * log(det(diag(N) - pa[2]*matw0)) -
   (1/(2*pa[1]))*
    t(Ystar0 - pa[3]*Y_1star - pa[2]*Ystar1- Xstar%*%as.matrix(pa[-c(1:3)]))%*%
	 (Ystar0 - pa[3]*Y_1star - pa[2]*Ystar1- Xstar%*%as.matrix(pa[-c(1:3)]))
  }
 
 if(estimation == 'numerical'){
  
 mle <- maxLik::maxLik(LL, start=c(stats::runif(1,0,10),
                         
						  stats::runif(1,-1,1),
						  stats::runif(1,-1,1),
 						  stats::rnorm(ncol(Xstar))),

  method='BFGS',hess=NULL)
  
 PA <- stats::coef(mle)	
 Parms <- c(PA[3],PA[2],PA[-c(1:3)],PA[1])
 names(Parms)[1:2] <- c(colnames(Y_1star),colnames(Ystar1))
 names(Parms)[-c(1,2,length(Parms))] <- colnames(Xstar)
 names(Parms)[length(Parms)] <- 'sigma2'
 rho <- Parms[2]
 Tau <- Parms[1]
 sigma2 <- Parms[length(Parms)]
 H <- mle$hessian
 Vars <- -H
 Vars0 <- solve(-H)
 maxLL <- mle[1]
 
 }else{

 A <- matrix(c(1,-1),nrow=2,ncol=1)
 B <- matrix(c(2,1),nrow=2,ncol=1)
 startingP <- stats::runif(n=1,min=-1,max=1)
 estRho <- maxLik::maxLik(llrho,start=startingP,
                 constraints=list(ineqA=A,ineqB=B),
                 method='BFGS',hess=NULL)
 
  
 rho <- stats::coef(estRho)
 maxLL <- estRho[1]
 if(length(rho)==0){
  rho <- 0
  maxLL <- NA 
 }
 
 
 beta0 <- par0
 beta1 <- par1
 betas <- beta0 - rho*beta1
 rownames(betas)[1] <- 'Y_1'
 names(betas) <- rownames(betas)
 
 parms <- solve(t(XXstar)%*%XXstar)%*%t(XXstar)%*%
  (Ystar0-rho*kronecker(diag(TT),matw0)%*%Ystar0)
 rownames(parms)[1] <- 'Y_1'
 
 
 
 sigma2 <- as.numeric((1/(N*TT))*t(e0-rho*e1)%*%(e0-rho*e1))
 
 
 Parms <- c(parms[1],rho,parms[-1],sigma2)
 names(betas) <- gsub('XXstar','',names(betas))
 names(Parms) <- c(names(betas)[1],'rho',names(betas)[-1],'sigma2')
 
 Y_1stara <- (Y_1star-mean(Y_1star))/stats::sd(Y_1star)
 
 ADJ_MAT <- kronecker(diag(TT),matw0)
 Wtilde <- matw0%*%solve(diag(N)-rho*matw0)
 var1 <- (1/sigma2)*t(Y_1star)%*%(Y_1star)
 var2 <- (1/sigma2)*t(Y_1star)%*%kronecker(diag(TT),Wtilde)%*%
   Y_1star*betas[1]
 var3 <- TT*matrixcalc::matrix.trace(Wtilde%*%Wtilde + t(Wtilde)%*%Wtilde) +
   (1/sigma2)*t(parms)%*%t(XXstar)%*%
   (kronecker(diag(TT),t(Wtilde)%*%Wtilde))%*%XXstar%*%parms
    var4 = (1/sigma2)*t(Xstar)%*%Y_1star
 
 var5 <- (1/sigma2)*t(Xstar)%*%kronecker(diag(TT),Wtilde)%*%
   Xstar%*%parms[-1]
 var6 <- (1/sigma2)*t(Xstar)%*%Xstar
 var7 <- (TT/sigma2)*matrixcalc::matrix.trace(Wtilde)
 
 var8 <- (N*TT)/2*((sigma2)^2)
 
 Vars <- matrix(0,nrow=length(betas)+2,ncol=length(betas)+2)
 Vars[1,1] <- var1
 Vars[2,1] <- var2; Vars[1,2] = var2
 Vars[2,2] <- var3
 Vars[3:(3+(length(parms[-1])-1)),1] <- var4 
 Vars[1,3:(3+(length(parms[-1])-1))] <- var4
 Vars[3:(3+(length(parms[-1])-1)),2] <- var5 
 Vars[2,3:(3+(length(parms[-1])-1))] <- var5 
 Vars[3:(3+(length(parms[-1])-1)),
  3:(3+(length(parms[-1])-1))] <- var6 
 Vars[nrow(Vars),2] <- var7
 Vars[2,nrow(Vars)] <- var7
 Vars[nrow(Vars),nrow(Vars)] <- var8
 Vars0 <- solve(Vars)
 
 Tau <- Parms[1]
 }
 std.err <- sqrt(diag(Vars0))
 
 Bias <- matrix(0,nrow=length(Parms),ncol=1)
 Bias[1] <- (1/N)*matrixcalc::matrix.trace(solve((1-Tau)*diag(N)-rho*matw0))  
 Bias[2] <- (1/N)*matrixcalc::matrix.trace(matw0%*%(diag(N)-rho*matw0)%*%
  solve((1-Tau)*diag(N)-rho*matw0))+
  (1/N)*matrixcalc::matrix.trace(matw0%*%(diag(N)-rho*matw0)) 
 Bias[nrow(Bias)] <- 1/(2*sigma2)
 
 if(corrBIAS==TRUE){
  BCLSDV <- Parms - solve((1/(N*TT))*(-Vars))%*%((1/TT)*(Bias))
 }else{
  BCLSDV <- Parms
 } 
 
 tab <- data.frame(
  variable = as.character(names(Parms)),
  estimate = round(BCLSDV,6),
  std.err = round(std.err,6),
  t.value = round(BCLSDV/std.err,6))
 tab$p.value <- round(2*stats::pt(-abs(tab$t.value),df=N*TT-nrow(tab)),6)
 rownames(tab) <- 1:nrow(tab)
 tab$variable <- as.character(tab$variable)
 tab$variable[1] <- paste('dm.',paste(Yvar,'_1',sep=''),sep='')
 tab$variable[2] <- paste('dm.spat.',Yvar,sep='')
 tab$signif <- ''
 tab$signif[tab$p.value < 0.1] = '.'
 tab$signif[tab$p.value < 0.05] = '*'
 tab$signif[tab$p.value < 0.01] = '**'
 tab$signif[tab$p.value < 0.001] = '***'
 
 tab2 <- data.frame(
  variable = names(Parms),
  estimate = round(Parms,6),
  std.err = round(std.err,6),
  t.value = round(Parms/std.err,6))
 tab2$p.value <- round(2*stats::pt(-abs(tab2$t.value),df=N*TT-nrow(tab2)),6)
 rownames(tab2) <- 1:nrow(tab2)
 
 yhat <- as.matrix(dataset0[,as.character(
   tab$variable[-length(tab$variable)])])%*%
   as.matrix(tab[-nrow(tab),2])
 
 
 uhat <- dataset0[[Yvar]] - yhat
 aic <- 2*(nrow(tab)-1)-2*as.numeric(maxLL)
 
 rm(tab2,Bias,dataset0,matw0,uhat,yhat,aic)
 out <- list(tab,maxLL)
 names(out) <- c('estimates','maxll')
 return(out)
}
   


#' Space-time bayesian INLA estimator
#'
#' This function estimates a space time linear model using the bayesian INLA. It is a wrapper of the INLA::inla function (Lindgren and Rue (2015) \doi{10.18637/jss.v063.i19}; Bivand, Gomez-Rubio and Rue (2015) \doi{10.18637/jss.v063.i20}) adapted to panel data.
#' 
#' 
#' @param  formula Formula of the model to be estimated
#' @param  d Data frame
#' @param  W Spatial matrix   
#' @param  RHO Parameter of spatial dependence
#' @param  PHI Parameter of temporal dependence
#' @param  var.agg Indexes of the panel dimensions. The first argument is the spatial dimension, the second argument is the temporal dimension.
#' @param  normalization Boolean. If TRUE the data are normalized before estimation
#' @param  improve Please refer to the documentation of the INLA package
#' @param  fhyper Plase refer to the documentation of the INLA package
#' @param  probit Plase refer to the documentation of the INLA package
#' @param  ... additional parameters. Please, refer to the documentation of the INLA package
#' 
#'
#' @return Returns a model of class "inla". Please, refer to the documentation of the INLA package for additional information
#' @examples 
#' \donttest{
#' 
#' set.seed(123)
#' sd = sim_data_fe(dataset=regsamp,N=100,TT=8,spatial = 80,
#'                  Tau = -0.2,Rho = 0.4, Beta = 2,sdDev = 2,
#'                  startingT = 10,LONGLAT = TRUE)
#' est_inla = inla.st(formula = Y~-1+X1,d = sd[[1]]@data,
#'                W = sd[[2]],PHI=-0.2,RHO=0.4,
#'                var.agg=c('Cod_Provincia','Anno'),
#'                family='gaussian',
#'                improve=TRUE,
#'                normalization=FALSE,
#'                control.family = list(hyper = list(prec=list(initial=25,fixed=TRUE))),
#'                control.predictor = list(compute = TRUE),
#'                control.compute = list(dic = TRUE, cpo = TRUE),
#'                control.inla = list(print.joint.hyper = TRUE))
#' summary(est_inla)
#' }
#'
#' @export
inla.st = function (formula, d, W, RHO,PHI,
    var.agg,     
    normalization=FALSE, 
    improve = TRUE,
    fhyper = NULL, 
    probit = FALSE, ...) 
{   
    
	if(normalization == TRUE){
	 novaragg <- stats::model.matrix(formula,d)
	 did <- d[,var.agg]
	 ydata <-  as.matrix(d[,as.character(formula)[2]])
   colnames(ydata) <- as.character(formula)[2]
	 novaragg <- cbind(ydata,novaragg)
	 d <- apply(novaragg,2,function(x) (x-mean(x))/stats::sd(x))
   d <- cbind(did,d)	
	}
	d <- d[order(d[[var.agg[2]]],d[[var.agg[1]]],decreasing=FALSE),]

	
	
	d$idx <- 1:nrow(d)
	
    N <- length(levels(as.factor(d[[var.agg[1]]])))
    TT <- nrow(d)/N
   
    iota <- rep(1,TT)
    Q <- kronecker(diag(TT) - (1/TT)*iota%*%t(iota),diag(N))
    
  
    mmatrix0 <- stats::model.matrix(formula, d)
	
	mmatrix <- mmatrix0
	
	IrhoWst <- effectsST(dataset=d,
    var.agg=var.agg[1],
    W=W,
    Tau=c(as.numeric(PHI)),
    Rho=c(as.numeric(RHO)),
    periods=TT)
	
	
    IrhoWst <- methods::as(IrhoWst, "sparseMatrix") 
   
	
	invIrhoWst <- solve(IrhoWst)
	
	
	IrhoW2 <- Matrix::crossprod(IrhoWst)
	
    
    assign("IrhoW2", IrhoW2, environment(formula))
    
	
    mm <- as.data.frame(as.matrix(invIrhoWst %*% mmatrix))
	mmSTAR = as.data.frame(Q %*% as.matrix(mm))
	
    names(mmSTAR) <- paste("x", 1:ncol(mmSTAR), sep = "")
    xnam <- names(mmSTAR)
	
    d2 <- cbind(d, mmSTAR)
    fmla <- paste(as.character(formula)[2], "~ -1+", paste(xnam, 
        collapse = "+"))
		
    if (is.null(fhyper)){
        fmla <- paste(fmla, "+f(idx, model=\"generic0\", Cmatrix=IrhoW2)", 
            sep = "")
    }else{ fmla <- paste(fmla, "+f(idx, model=\"generic0\", Cmatrix=IrhoW2, hyper=fhyper)", 
        sep = "")
	}	
		
    fmla <- stats::as.formula(fmla)
    res <- INLA::inla(fmla, data = d2, ...)
    
	if (improve){
     res <- INLA::inla.rerun(res)
    }
	
	
     res$logdet <- as.numeric(Matrix::determinant(IrhoW2)$modulus)
     res$mlik <- res$mlik + res$logdet/2
	 
     res$impacts <- FALSE
    
	
    return(res)
}


  
getYtilde <- function(dataset,yvar,xvar,wmat,var.agg,Rho,Tau){
 var.agg0 <- 'Anno'
 yvar1 <- paste(yvar,'_1',sep='')
 datalag <- fdiff(dataset,VAR=yvar,var.agg=var.agg)[,c(1,2,4)]
 dataset0 <- plyr::join(dataset,datalag,type='right')
 N <- nrow(wmat)
 TT <- nrow(dataset)/N
 datalagsX <- list()
 Anno1 <- min(dataset$Anno)
 AnnoT <- max(dataset$Anno)
 set_Annos <- Anno1:AnnoT
 dataX <- subset(dataset,dataset$Anno == set_Annos[1])
 xs <- xvar
 for(i in 2:length(set_Annos)){
  temp <- subset(dataset,dataset$Anno == set_Annos[i])
  names(temp)[which(!names(temp) %in% var.agg)] <- 
   paste(names(temp)[which(!names(temp) %in% var.agg)],i-1,sep='_')
  temp[[var.agg0]] <- Anno1
  dataX <- plyr::join(dataX,temp,by=var.agg)
  xs <- c(xs,paste(xvar,i-1,sep='_'))
 }
 dataX0 <- cbind(1,dataX[,xs])
 names(dataX0)[1] <- 'const'
 nAlphas <- ncol(dataX0)
 err_init <- (diag(N)-Rho*wmat)%*%as.matrix(dataX[,yvar]) 
 
 split_data <- split(dataset0,f=datalag[[var.agg0]])
 N <- nrow(split_data[[1]])
 err_data <- lapply(split_data,
  function(w) (diag(N)-Rho*wmat)%*%as.matrix(w[,yvar]) -
                 Tau * as.matrix(w[,yvar1]) )
 err_tab <- do.call('rbind',err_data)	
 
 err_tab1 <- rbind(err_init,err_tab)
 return(err_tab1) 
} 
 	

getXtildeM <- function(dataset,yvar,xvarm,xvar,wmat,var.agg,Rho){
 
 var.agg0 <- 'Anno'
 yvar1 <- paste(yvar,'_1',sep='')
 N <- nrow(wmat)
 TT <- nrow(dataset)/N+1
 Anno1 <- min(dataset$Anno)
 AnnoT <- max(dataset$Anno)
 set_Annos <- Anno1:AnnoT
 dataX <- subset(dataset,dataset$Anno == set_Annos[1])
 dataX <- dataX[,names(dataX) %in% xvar]
 
 Xtilde <- matrix(0,nrow=(nrow(dataX)*(TT-1)),
                   ncol=1+((TT-1)*length(xvarm)+length(xvar)))
 posI <- 1:N	
 posX2 <-  1:ncol(Xtilde)
 posX3 <- posX2[!posX2 %in% c(1:(1+(TT-1)*length(xvarm)))]
 Xtilde[posI,1] <- 1
 Xtilde[posI,(1+1):(1+length(xvar))] <- as.matrix(dataX[,which(!names(dataX) %in% c(var.agg,yvar))])
 
 
 for(i in 2:(length(set_Annos))){
  temp0 <- subset(dataset,dataset$Anno == set_Annos[i])
  temp <- temp0[,names(temp0) %in% xvar]
  temp00 <- temp0[,names(temp0) %in% xvarm]
  
  if(length(posX3)>1){
   temp1 <- as.matrix(temp[,which(!names(temp) %in%  c(var.agg,yvar))])
   temp10 <- as.matrix(temp00[,which(!names(temp00) %in%  c(var.agg,yvar))])

   lx <- 1+length(xvarm)*(i-1)+1 
   lx2 <- lx+length(xvarm)-1
   
  }else{
   temp1 <- temp[,which(!names(temp) %in%  c(var.agg,yvar))]
   lx <- 1+length(xvarm)*i
   lx2 <- lx
 
  } 
  
  Xtilde[posI,lx:lx2] <- temp10
  Xtilde[((i-1)*N+1):(i*N),posX3] <- temp1
 }
 
 corX <- stats::cor(Xtilde)
 dupX <- which(duplicated(corX[,1]))
 if(length(dupX)>0){
 Xtilde2 <- Xtilde[,-dupX]
 }else{
 Xtilde2 <- Xtilde
 }
 
 return(Xtilde2) 
} 
 		
		

uml_err_fullM <- function(dataset,yvar,xvarm,xvar,wmat,var.agg,Rho,Tau,Beta,Alphas){
 
 var.agg0 <- 'Anno'
 yvar1 <- paste(yvar,'_1',sep='')
 N <- nrow(wmat)
 datalag <- fdiff(dataset,VAR=yvar,var.agg=var.agg)[,c(1,2,4)]
 dataset0 <- plyr::join(dataset,datalag,type='right')
 datalagsX <- list()
 Anno1 <- min(dataset$Anno)
 AnnoT <- max(dataset$Anno)
 set_Annos <- Anno1:AnnoT
 dataX <- subset(dataset,dataset$Anno == set_Annos[1])
 dataX <- dataX[,names(dataX) %in% c(xvar,var.agg,yvar)] 
 xs <- xvar
 
 for(i in 2:length(set_Annos)){
  temp <- subset(dataset,dataset$Anno == set_Annos[i])
  names(temp)[which(!names(temp) %in% var.agg)] <- 
   paste(names(temp)[which(!names(temp) %in% var.agg)],i-1,sep='_')
  temp[[var.agg0]] <- Anno1
  dataX <- plyr::join(dataX,temp,by=var.agg)
  xs <- c(xs,paste(xvar,i-1,sep='_'))
 }
 
 less2 <- c(grep('_t1',xs),grep('_lag',xs))
 if(length(less2) > 0){
 xs <- xs[-less2]
 }else{
 xs <- xs
 }
 
 dataX0 <- cbind(1,dataX[,xs])
 names(dataX0)[1] <- 'const'
 nAlphas <- ncol(dataX0)
 err_init <- (diag(N)-Rho*wmat)%*%as.matrix(dataX[,yvar]) - as.matrix(dataX0)%*%(Alphas)
 
 
 split_data <- split(dataset0,f=datalag[[var.agg0]])
 N <- nrow(split_data[[1]])
 err_data <- lapply(split_data,
  function(w) (diag(N)-Rho*wmat)%*%as.matrix(w[,yvar]) -
                 Tau * as.matrix(w[,yvar1]) -
                 as.matrix(w[,xvar]) %*% as.matrix(Beta))
 err_tab <- do.call('rbind',err_data)	
 
 err_tab1 <- rbind(err_init,err_tab)
 return(err_tab1) 
}  


getPI <- function(wmat,Tau,Rho){
 N <- nrow(wmat)
 PI <- Tau*solve(diag(N) - Rho*wmat)
 return(PI)
}	  


getV <- function(PI,m){
 N <- nrow(PI)
 PIm <- PI
 for(i in 1:(2*m-1)){
  PIm <- PIm %*%PI
 }
 V <- 2*solve(diag(N)+PI)%*%(diag(N) + PIm)
 return(V)
}


getVtheta = function(Sigma2,Sigma2xi,V){
 N <- nrow(V)
 theta <- (Sigma2/Sigma2xi)
 Vtheta <- (theta*diag(N)+V)
 return(Vtheta) 
}


getHv <- function(Vtheta,N,TT){
 Hv <- diag(N*TT)*2
 for(i in 1:(TT-1)){
  Hv[(1:N)+N*i,(1:N)+N*(i-1)] <- -diag(N)
  Hv[(1:N)+N*(i-1),(1:N)+N*i] <- -diag(N)
 }
 Hv[1:N,1:N] <- Vtheta
 return(Hv)
}		 
	 
	 

uml_data <- function(dataset,ff,var.agg){
 y <- gsub('//(//)','',ff[2])
 x0 <- gsub('//(//)','',ff[3])
 x0 <- gsub(' ','',x0)
 x <- strsplit(x0,split='\\+')[[1]]
 z <- c(y,x)
 fdata <- fdiff(dataset,VAR=z[1],var.agg=var.agg)[,c(1,2,5)]
 for(i in 2:length(z)){
  fdata <- plyr::join(fdata,fdiff(dataset,VAR=z[i],var.agg=var.agg)[,c(1,2,5)])
 }
 return(fdata)
}



effectsST <- function(dataset,
 var.agg,
 W,Tau,
 Rho,
 periods=NULL){
 
  
 N <- length(levels(as.factor(dataset[[var.agg]])))
 if(is.null(periods)){
  TT <- length(min(dataset$Anno):max(dataset$Anno))
 }else{
  TT <- periods
 }
 
 C <- -(diag(N)*Tau)
 B <- (diag(N) - Rho*W)
 EFF <- matrix(0,ncol=N*TT,nrow=N*TT)
 seqB1 <- list()
 seqB2 <- list()
 seqC1 <- list()
 seqC2 <- list()
 for(i in 1:TT){
  seqB1[[i]] <- (1+(N*(i-1))) 
  seqB2[[i]] <- N*i
  seqC1[[i]] <- (1+(N*(i)))
  seqC2[[i]] <- N*(i+1)
 }
 for(j in 1:(TT)){
  EFF[seqB1[[j]]:seqB2[[j]],
      seqB1[[j]]:seqB2[[j]]] <- B
 }
 for(k in 1:(TT-1)){ 
  EFF[seqC1[[k]]:seqC2[[k]],
      seqB1[[k]]:seqB2[[k]]] <- C
 }
 return(EFF)
}

#' MML estimator
#'
#' This function estimates a space time linear model according to the specified formula using the ML estimator as in Elhorst (2010) \doi{10.1016/j.regsciurbeco.2010.03.003}. The estimator maximizes the full log-likelihood function in which the parameter of spatial dependence is constrained.
#' 
#' @param Rho the constrained parameter of spatial dependence
#' @param ff Formula of the linear model. It excludes the spatial lag
#' @param dataset Data frame with the data
#' @param wmat Spatial weight matrix
#' @param var.agg Spatial index of the data frame
#' @param m How many time periods have passed since the beginning of the space-time process
#'
#' @return The estimates tables
#' @examples 
#' \donttest{
#' 
#' set.seed(123)
#' sd = sim_data_fe(dataset=regsamp,N=50,TT=8,
#'                 spatial = 80,Tau = -0.2,Rho = 0.4,
#'                 Beta = 2,sdDev = 2,startingT = 10,
#'                 LONGLAT = TRUE);sd[[1]]$X2 = stats::rnorm(nrow(sd[[1]]@data))
#' est_mml = mml(dataset = sd[[1]]@data,Rho = 0.4,
#'               ff = Y~X1+X2,
#'               wmat = sd[[2]],var.agg = c('Anno','Cod_Provincia'),
#'               m = 10)
#' est_mml
#' }
#'
#' @export
mml = function(Rho,
 ff,dataset,wmat,var.agg,m=10){
 
 
 dataset0 <- uml_data(
             dataset=dataset,
			 ff=ff,
			 var.agg=var.agg)
 
 N <- nrow(wmat)
 TT <- nrow(dataset)/N

 yvar <- gsub('//(//)','',ff[2])
 x0 <- gsub('//(//)','',ff[3])
 x0 <- gsub(' ','',x0)
 xvar <- strsplit(x0,split='\\+')[[1]]
 yvar0 <- paste('diff_',yvar,sep='')
 yvar1 <- paste(yvar0,'_1',sep='')
 xvar0 <- paste('diff_',xvar,sep='')
 
 less1 <- c(grep('_t1',xvar0),grep('_lag',xvar0))
 if(length(less1)>0){
 xvar0m <- xvar0[-less1] 
 }else{
 xvar0m <- xvar0
 }
 
 
 
 LL <- function(pa){
    TAU = pa[1]
    SIGMA2 = pa[2]
    SIGMA2XI = pa[3]
  
	 ytilde <- getYtilde(dataset=dataset0,
                          yvar=yvar0,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAU)
 
 xtilde <- getXtildeM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm=xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho)		
 
 
 PI <- getPI(wmat=wmat,Tau=TAU,Rho=Rho) 						  
 V <- getV(PI=PI,m=m)						  
 Vtheta <- getVtheta(Sigma2=SIGMA2,Sigma2xi=SIGMA2XI,V=V)
 Hv <- getHv(Vtheta=Vtheta,N=N,TT=TT-1)										  
 

  rm(BETAS)
  rm(coeff2)
  invHv <- solve(Hv)
 
  
  
  coeff2 <- try(solve(t(xtilde)%*%invHv%*%xtilde)%*%
   t(xtilde)%*%invHv%*%ytilde)
   
  whichNO <- which(apply(xtilde,2,function(x) sum(x) ==0)) 
 
  names(coeff2)[1:length(coeff2)] <- 'beta'
  names(coeff2)[1:(length(coeff2)-length(xvar))] <- 'alpha'
  alphas2 <- coeff2[which(names(coeff2) == 'alpha')]
  betas2 <- coeff2[which(names(coeff2) == 'beta')]
  
 derr_full <- uml_err_fullM(dataset=dataset0,
                          yvar= yvar0,
						  xvarm = xvar0m, 
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAU,
						  Beta=betas2,
						  Alphas=alphas2)
 
 
 	 
    ll <-  -(1/2)*N*(TT) * log(2*pi*SIGMA2) + 
        (TT)*log(det(diag(N) - Rho*wmat)) -
        (1/2)*log(det(Hv)) -  
        (1/(2*SIGMA2))*t(derr_full)%*%invHv%*%derr_full
    

    if(!is.finite(as.numeric(ll))){ll <- -1.0e+10}
	
	print('------------------')	
	print('parameters step1')
	print(paste('TAU:',TAU))
	print(paste('SIGMA2:',SIGMA2))
	print(paste('SIGMA2XI:',SIGMA2XI))
	print(paste('log-likelihood:',ll))	
		
	return(ll)	
  }
 
  constrTAU <- 1-(abs(Rho)+0.05)
  constrTAU <- 1-(abs(Rho)+0.2)
  A <- matrix(c(-1,1,0,0,0,0,1,0,0,0,0,1),nrow=4)

  B <- matrix(c(constrTAU,-0.001,-0.0001,-0.0001))

 mle <- maxLik::maxLik(LL, start=c(0.002,0.1,0.1),
   constraints=list(ineqA=A,ineqB=B),
  method='BFGS',hess=NULL)
  
  
 TAUhat <-  mle$estimate[1]
 SIGMA2hat <- mle$estimate[2]
 SIGMA2XIhat <- mle$estimate[3]
 curenv = environment() 

 
 
 LL2 <- function(pa){
    TAU = pa[1]
	print('------------------')	
	print('parameters step2')
	print(paste('TAU:',TAU))
	
  PI <- getPI(wmat=wmat,Tau=TAU,Rho=Rho) 						  
  V <- getV(PI=PI,m=m)	
  Vtheta <- getVtheta(Sigma2=SIGMA2hat,Sigma2xi=SIGMA2XIhat,V=V)
  Hv <- getHv(Vtheta=Vtheta,N=N,TT=TT-1)	
 
  ytilde <- getYtilde(dataset=dataset0,
                          yvar=yvar0,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAU)
						  
  xtilde <- getXtildeM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm = xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho)		
 
  invHv <- solve(Hv)


  coeff2 <- try(solve(t(xtilde)%*%invHv%*%xtilde)%*%
   t(xtilde)%*%invHv%*%ytilde)
  whichNO <- which(apply(xtilde,2,function(x) sum(x) ==0)) 

  
  

  names(coeff2)[1:length(coeff2)] <- 'beta'
  names(coeff2)[1:(length(coeff2)-length(xvar))] <- 'alpha'
  alphas2 <- coeff2[which(names(coeff2) == 'alpha')]
  betas2 <- coeff2[which(names(coeff2) == 'beta')]
  
 derr_full <- uml_err_fullM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm = xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAU,
						  Beta=betas2,
						  Alphas=alphas2)
 
  sigma2_glob <- (t(derr_full)%*%invHv%*%derr_full)/(N*TT)
 
 
 
      ll <- -(1/2)*N*(TT) * log(2*pi*sigma2_glob) + 
        (TT)*log(det(diag(N) - Rho*wmat)) -
        (1/2)*log(det(Hv)) -  
        (1/(2*sigma2_glob))*t(derr_full)%*%invHv%*%derr_full
 
       #assign('sigma2_glob',sigma2_glob,envir=curenv)
 
		    if(!is.finite(as.numeric(ll))){ll=-1.0e+10}

	return(ll)	
  }
	
 
 LL3 <- function(pa){
    TAU = pa[1]
	print('------------------')	
	print('parameters step2')
	print(paste('TAU:',TAU))
	
  PI <- getPI(wmat=wmat,Tau=TAU,Rho=Rho) 						  
  V <- getV(PI=PI,m=m)	
  Vtheta <- getVtheta(Sigma2=SIGMA2hat,Sigma2xi=SIGMA2XIhat,V=V)
  Hv <- getHv(Vtheta=Vtheta,N=N,TT=TT-1)	
 
  ytilde <- getYtilde(dataset=dataset0,
                          yvar=yvar0,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAU)
						  
  xtilde <- getXtildeM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm = xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho)		
 
  invHv <- solve(Hv)


  coeff2 <- try(solve(t(xtilde)%*%invHv%*%xtilde)%*%
   t(xtilde)%*%invHv%*%ytilde)
  whichNO <- which(apply(xtilde,2,function(x) sum(x) ==0)) 

  
  

  names(coeff2)[1:length(coeff2)] <- 'beta'
  names(coeff2)[1:(length(coeff2)-length(xvar))] <- 'alpha'
  alphas2 <- coeff2[which(names(coeff2) == 'alpha')]
  betas2 <- coeff2[which(names(coeff2) == 'beta')]
  
 derr_full <- uml_err_fullM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm = xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAU,
						  Beta=betas2,
						  Alphas=alphas2)
 
  sigma2_glob <- (t(derr_full)%*%invHv%*%derr_full)/(N*TT)
 
	return(sigma2_glob)	
  }
	
  constrTAU <- 1-(abs(Rho)+0.05)
  A2 <- matrix(c(-1,1),nrow=2)
  B2 <- matrix(c(constrTAU,constrTAU))
 
 mle2 <- maxLik::maxLik(LL2, start=c(TAUhat),
   constraints=list(ineqA=A2,ineqB=B2),
  method='BFGS',hess=NULL)
  
  
 TAUhat <-  mle2$estimate[1]
 
 ytilde <- getYtilde(dataset=dataset0,
                          yvar=yvar0,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAUhat)
 xtilde <- getXtildeM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm = xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho)		
 
 PI <- getPI(wmat=wmat,Tau=TAUhat,Rho=Rho) 						  
 V <- getV(PI=PI,m=m)
 
 sigma2_glob <- LL3(TAUhat)						  
 Vtheta <- getVtheta(Sigma2=as.numeric(sigma2_glob),Sigma2xi=as.numeric(SIGMA2XIhat),V=V)
 Hv <- getHv(Vtheta=Vtheta,N=N,TT=TT-1)
  
  invHv <- solve(Hv)

  coeff2 <- try(solve(t(xtilde)%*%invHv%*%xtilde)%*%
   t(xtilde)%*%invHv%*%ytilde)
  whichNO <- which(apply(xtilde,2,function(x) sum(x) ==0)) 

  
  

  names(coeff2)[1:length(coeff2)] <- 'beta'
  names(coeff2)[1:(length(coeff2)-length(xvar))] <- 'alpha'
  alphas2 <- coeff2[which(names(coeff2) == 'alpha')]
  betas2 <- coeff2[which(names(coeff2) == 'beta')]
  
 derr_full <- uml_err_fullM(dataset=dataset0,
                          yvar=yvar0,
						  xvarm = xvar0m,
						  xvar=xvar0,
						  wmat=wmat,
						  var.agg=var.agg,
						  Rho=Rho,
						  Tau=TAUhat,
						  Beta=betas2,
						  Alphas=alphas2)
 rm(sigma2_glob)
 
 invHv <- solve(Hv)


 sigma2 <- (t(derr_full)%*%invHv%*%derr_full)/(N*TT)

  
   errors <- ytilde - xtilde%*%coeff2 
  coeff2varM <- as.numeric(t(errors)%*%errors) *
   1/(N*TT-length(coeff2))*solve(t(xtilde)%*%invHv%*%xtilde)
  coeff2var <- sqrt(diag(coeff2varM))
  names(coeff2var)[1:length(coeff2var)] <- 'beta'
  names(coeff2var)[1:(length(coeff2var)-length(xvar))] <- 'alpha'
  alphas2var <- coeff2var[which(names(coeff2var) == 'alpha')]
  betas2sd <- coeff2var[which(names(coeff2var) == 'beta')]
  names(betas2sd) <- xvar
  betas2sd <- betas2sd
  
  COEF <- data.frame(variable=yvar,
                     estimate=summary(mle2)[6]$estimate[1,1],
					 std.err=summary(mle2)[6]$estimate[1,2])
  
  BETAS <- data.frame(variable=xvar,
                     estimate=betas2,
					 std.err=betas2sd)
  BETAS <- rbind(COEF,BETAS)
  rownames(BETAS) <- 1:nrow(BETAS)
  BETAS$t.value <- BETAS$estimate/BETAS$std.err					 
  rownames(BETAS) <- 1:nrow(BETAS)
  BETAS$p.value <- round(2*stats::pt(-abs(BETAS$t.value),df=N*TT-length(coeff2)),6)	
  BETAS$signif <- ''
  BETAS$signif[BETAS$p.value<0.1] <- '.'
  BETAS$signif[BETAS$p.value<0.05] <- '*'
  BETAS$signif[BETAS$p.value<0.01] <- '**'
  BETAS$signif[BETAS$p.value<0.001] <- '***'
  

  out <- list(BETAS,mle,mle2,sigma2,coeff2)
  names(out) <- c('coefs','mle','mle2','sigma2','coeffs')  
 return(out) 
}
  
  
  
getYtilde <- function(dataset,yvar,xvar,wmat,var.agg,Rho,Tau){
 var.agg0 <- 'Anno'
 yvar1 <- paste(yvar,'_1',sep='')
 datalag <- fdiff(dataset,VAR=yvar,var.agg=var.agg)[,c(1,2,4)]
 dataset0 <- plyr::join(dataset,datalag,type='right')
 N <- nrow(wmat)
 TT <- nrow(dataset)/N
 datalagsX <- list()
 Anno1 <- min(dataset$Anno)
 AnnoT <- max(dataset$Anno)
 set_Annos <- Anno1:AnnoT
 dataX <- subset(dataset,dataset$Anno == set_Annos[1])
 xs <- xvar
 for(i in 2:length(set_Annos)){
  temp <- subset(dataset,dataset$Anno == set_Annos[i])
  names(temp)[which(!names(temp) %in% var.agg)] <- 
   paste(names(temp)[which(!names(temp) %in% var.agg)],i-1,sep='_')
  temp[[var.agg0]] <- Anno1
  dataX <- plyr::join(dataX,temp,by=var.agg)
  xs <- c(xs,paste(xvar,i-1,sep='_'))
 }
 dataX0 <- cbind(1,dataX[,xs])
 names(dataX0)[1] <- 'const'
 nAlphas <- ncol(dataX0)
 err_init <- (diag(N)-Rho*wmat)%*%as.matrix(dataX[,yvar]) 
 
 split_data <- split(dataset0,f=datalag[[var.agg0]])
 N <- nrow(split_data[[1]])
 err_data <- lapply(split_data,
  function(w) (diag(N)-Rho*wmat)%*%as.matrix(w[,yvar]) -
                 Tau * as.matrix(w[,yvar1]) )
 err_tab <- do.call('rbind',err_data)	
 
 err_tab1 <- rbind(err_init,err_tab)
 return(err_tab1) 
} 
 	

getXtildeM <- function(dataset,yvar,xvarm,xvar,wmat,var.agg,Rho){
 
 var.agg0 <- 'Anno'
 yvar1 <- paste(yvar,'_1',sep='')
 N <- nrow(wmat)
 TT <- nrow(dataset)/N+1
 Anno1 <- min(dataset$Anno)
 AnnoT <- max(dataset$Anno)
 set_Annos <- Anno1:AnnoT
 dataX <- subset(dataset,dataset$Anno == set_Annos[1])
 dataX <- dataX[,names(dataX) %in% xvar]
 
 Xtilde <- matrix(0,nrow=(nrow(dataX)*(TT-1)),
                   ncol=1+((TT-1)*length(xvarm)+length(xvar)))
 posI <- 1:N	
 posX2 <-  1:ncol(Xtilde)
 posX3 <- posX2[!posX2 %in% c(1:(1+(TT-1)*length(xvarm)))]
 Xtilde[posI,1] <- 1
 Xtilde[posI,(1+1):(1+length(xvar))] <- as.matrix(dataX[,which(!names(dataX) %in% c(var.agg,yvar))])
 
 
 for(i in 2:(length(set_Annos))){
  temp0 <- subset(dataset,dataset$Anno == set_Annos[i])
  temp <- temp0[,names(temp0) %in% xvar]
  temp00 <- temp0[,names(temp0) %in% xvarm]
  
  if(length(posX3)>1){
   temp1 <- as.matrix(temp[,which(!names(temp) %in%  c(var.agg,yvar))])
   temp10 <- as.matrix(temp00[,which(!names(temp00) %in%  c(var.agg,yvar))])

   lx <- 1+length(xvarm)*(i-1)+1 
   lx2 <- lx+length(xvarm)-1
   #print(paste('lx:',lx))
   #print(paste('lx2:',lx2))
  }else{
   temp1 <- temp[,which(!names(temp) %in%  c(var.agg,yvar))]
   lx <- 1+length(xvarm)*i
   lx2 <- lx
  # print(paste('lx:',lx))
   #print(paste('lx2:',lx2))
  } 
  
  Xtilde[posI,lx:lx2] <- temp10
  Xtilde[((i-1)*N+1):(i*N),posX3] <- temp1
 }
 
 corX <- stats::cor(Xtilde)
 dupX <- which(duplicated(corX[,1]))
 if(length(dupX)>0){
 Xtilde2 <- Xtilde[,-dupX]
 }else{
 Xtilde2 <- Xtilde
 }
 
 return(Xtilde2) 
} 
 		
		

uml_err_fullM <- function(dataset,yvar,xvarm,xvar,wmat,var.agg,Rho,Tau,Beta,Alphas){
 
 var.agg0 <- 'Anno'
 yvar1 <- paste(yvar,'_1',sep='')
 N <- nrow(wmat)
 datalag <- fdiff(dataset,VAR=yvar,var.agg=var.agg)[,c(1,2,4)]
 dataset0 <- plyr::join(dataset,datalag,type='right')
 datalagsX <- list()
 Anno1 <- min(dataset$Anno)
 AnnoT <- max(dataset$Anno)
 set_Annos <- Anno1:AnnoT
 dataX <- subset(dataset,dataset$Anno == set_Annos[1])
 dataX <- dataX[,names(dataX) %in% c(xvar,var.agg,yvar)] 
 xs <- xvar
 
 for(i in 2:length(set_Annos)){
  temp <- subset(dataset,dataset$Anno == set_Annos[i])
  names(temp)[which(!names(temp) %in% var.agg)] <- 
   paste(names(temp)[which(!names(temp) %in% var.agg)],i-1,sep='_')
  temp[[var.agg0]] <- Anno1
  dataX <- plyr::join(dataX,temp,by=var.agg)
  xs <- c(xs,paste(xvar,i-1,sep='_'))
 }
 
 less2 <- c(grep('_t1',xs),grep('_lag',xs))
 if(length(less2) > 0){
 xs <- xs[-less2]
 }else{
 xs <- xs
 }
 
 dataX0 <- cbind(1,dataX[,xs])
 names(dataX0)[1] <- 'const'
 nAlphas <- ncol(dataX0)
 err_init <- (diag(N)-Rho*wmat)%*%as.matrix(dataX[,yvar]) - as.matrix(dataX0)%*%(Alphas)
 
 
 split_data <- split(dataset0,f=datalag[[var.agg0]])
 N <- nrow(split_data[[1]])
 err_data <- lapply(split_data,
  function(w) (diag(N)-Rho*wmat)%*%as.matrix(w[,yvar]) -
                 Tau * as.matrix(w[,yvar1]) -
                 as.matrix(w[,xvar]) %*% as.matrix(Beta))
 err_tab <- do.call('rbind',err_data)	
 
 err_tab1 <- rbind(err_init,err_tab)
 return(err_tab1) 
}  


getPI <- function(wmat,Tau,Rho){
 N <- nrow(wmat)
 PI <- Tau*solve(diag(N) - Rho*wmat)
 return(PI)
}	  


getV <- function(PI,m){
 N <- nrow(PI)
 PIm <- PI
 for(i in 1:(2*m-1)){
  PIm <- PIm %*%PI
 }
 V <- 2*solve(diag(N)+PI)%*%(diag(N) + PIm)
 return(V)
}


getVtheta = function(Sigma2,Sigma2xi,V){
 N <- nrow(V)
 theta <- (Sigma2/Sigma2xi)
 Vtheta <- (theta*diag(N)+V)
 return(Vtheta) 
}


getHv <- function(Vtheta,N,TT){
 Hv <- diag(N*TT)*2
 for(i in 1:(TT-1)){
  Hv[(1:N)+N*i,(1:N)+N*(i-1)] <- -diag(N)
  Hv[(1:N)+N*(i-1),(1:N)+N*i] <- -diag(N)
 }
 Hv[1:N,1:N] <- Vtheta
 return(Hv)
}		 
	 
	 

uml_data <- function(dataset,ff,var.agg){
 y <- gsub('//(//)','',ff[2])
 x0 <- gsub('//(//)','',ff[3])
 x0 <- gsub(' ','',x0)
 x <- strsplit(x0,split='\\+')[[1]]
 z <- c(y,x)
 fdata <- fdiff(dataset,VAR=z[1],var.agg=var.agg)[,c(1,2,5)]
 for(i in 2:length(z)){
  fdata <- plyr::join(fdata,fdiff(dataset,VAR=z[i],var.agg=var.agg)[,c(1,2,5)])
 }
 return(fdata)
}



effectsST <- function(dataset,
 var.agg,
 W,Tau,
 Rho,
 periods=NULL){
 # gives matrix Q from paper Debarsky,
 # Ertur, LeSage
  
 N <- length(levels(as.factor(dataset[[var.agg]])))
 if(is.null(periods)){
  TT <- length(min(dataset$Anno):max(dataset$Anno))
 }else{
  TT <- periods
 }
 
 C <- -(diag(N)*Tau)
 B <- (diag(N) - Rho*W)
 EFF <- matrix(0,ncol=N*TT,nrow=N*TT)
 seqB1 <- list()
 seqB2 <- list()
 seqC1 <- list()
 seqC2 <- list()
 for(i in 1:TT){
  seqB1[[i]] <- (1+(N*(i-1))) 
  seqB2[[i]] <- N*i
  seqC1[[i]] <- (1+(N*(i)))
  seqC2[[i]] <- N*(i+1)
 }
 for(j in 1:(TT)){
  EFF[seqB1[[j]]:seqB2[[j]],
      seqB1[[j]]:seqB2[[j]]] <- B
 }
 for(k in 1:(TT-1)){ 
  EFF[seqC1[[k]]:seqC2[[k]],
      seqB1[[k]]:seqB2[[k]]] <- C
 }
 return(EFF)
}


#' Simulate space-time stochastic process with fixed-effect
#'
#' This function simulates a space-time stochastic process according to the defined spatial structure and input paramters. It simulates data of a dynamic spatial lag model. It includes one exogenous variable and a fixed-effect correlated with the exogenous variable. 
#' 
#' @param dataset SpatialObject with the spatial units for which the data will be simulated
#' @param N How many spatial units will be used
#' @param TT Time dimension of the simulated process
#' @param spatial Radius that defines the scope of spatial dependence
#' @param Tau Autocorrelation parameter
#' @param Rho Spatial dependence parameter
#' @param Beta Coefficient associated to the exogenous variable
#' @param sdDev Standard Deviation of the (gaussian) error term
#' @param startingT The number of time periods after which the simulated data will be recorded
#' @param LONGLAT Boolean. If the projection is longlat
#'
#' @return A list with two objects. The first object is the STFDF with the simulated data. The second object is the spatial weight matrix
#' @examples
#' \donttest{
#' library(spacetime)
#' library(sp)
#' library(spdep)
#' set.seed(123)
#' sd = sim_data_fe(dataset=regsamp,N=100,TT=8,
#'                  spatial = 80,Tau = -0.2,Rho = 0.4,
#'                  Beta = 2,sdDev = 2,startingT = 10,
#'                  LONGLAT = TRUE)
#' stplot(sd[[1]][,,'Y'])
#' dev.new()
#' plot(sel_regioni)
#' points(coordinates(sd[[1]]@sp))
#' plot(mat2listw(sd[[2]]),coordinates(sd[[1]]@sp),add=TRUE,col=2)
#' }
#'
#' @export
sim_data_fe = function(dataset,N,TT,spatial=100,
 Tau=-0.14,Rho=0.67,Beta=1,sdDev=5,
 startingT = 11,LONGLAT=TRUE){


 
N = N
# prendo un'osservazione in piu' per poterla poi
# eiminare
T0 = TT
TT = TT+startingT
# prendo le coordinate
prova = sp::spTransform(dataset[1:N,],
 sp::CRS("+proj=longlat +datum=WGS84"))

if(is(dataset,"SpatialPointsDataFrame")){ 
 Coordinates = sp::coordinates(prova)
 spat = sp::SpatialPoints(Coordinates)
 sp::proj4string(spat) = sp::proj4string(prova)
}else{
 Coordinates = sp::coordinates(prova@sp)
 spat = prova@sp
}


if(LONGLAT){
spatial_conv = spatial
}else{
conv_unit = 30/0.358
spatial_conv  = spatial/conv_unit
}

netw = spdep::dnearneigh(Coordinates,d1=0,d2=spatial_conv,longlat=LONGLAT)

matw = spdep::nb2mat(netw,zero.policy=TRUE)

Tau = Tau
Rho = Rho
Beta = Beta
listX = list()
listX1 = list()
listU = list()
# costruisco matrice (I - rho*W)
w = kronecker(diag(TT),matw)
w = Matrix::Matrix(w,sparse=TRUE)
# costruisco matrice (I - rho*W)^(-1)
W = as.matrix(solve(diag(N*TT)  - Rho*w))



Sigma = W
listN = list()
listFE = list()
# creo la prima osservazione
for(i in 1:N){
 listN[[i]] = rep(0,TT)
 listN[[i]][1] = stats::runif(1,-2,2)
 listU[[i]] = rep(0,TT)
 listU[[i]] = stats::rnorm(TT,mean=0,sd=sdDev)
 listFE[[i]] = stats::runif(1,-10,10)
 listX[[i]] = stats::arima.sim(list(ar=0.02),TT)
 listX1[[i]] = listX[[i]]+0.3*listFE[[i]]
}


# per ogni periodo temporale, prendo ongi soggetto e 
# creo i dati in questo modo:

# y_11 = (I - rho*W)^(-1) %*% [y_10,y_20,y_30, ... ,y_n0] * Tau +
#        (I - rho*W)^(-1) %*% [x_11,x_21,x_31, ... ,x_n1] * Beta +
#        (I - rho*W)^(-1) %*% [e_11,e_21,e_31, ... ,e_n1] +
#        (I - rho*W)^(-1) %*% FE
idxN = 1    
idxT = 1
for(tt in 2:TT){  
  for(j in 1:N){
   listN[[j]][tt] = Sigma[idxN,idxT:(idxT+N-1)] %*% as.matrix(unlist(lapply(listN, function(x) x[tt-1]))) * Tau +
                    Sigma[idxN,idxT:(idxT+N-1)] %*% as.matrix(unlist(lapply(listX1, function(x) x[tt]))) * Beta +
                    Sigma[idxN,idxT:(idxT+N-1)] %*% as.matrix(unlist(lapply(listU, function(x) x[tt]))) + 
                                        Sigma[idxN,idxT:(idxT+N-1)] %*% as.matrix(unlist(listFE))
                                        
                                        
   idxN = idxN + 1                                      
 }
 idxT = idxT + N
} 

Y = unlist(listN)
X = unlist(listX1)
U = unlist(listU)
I = rep(1:N,each=TT)
A = rep(1:TT,N)

Data  = data.frame(
 Cod_Provincia = I,
 Anno = A,
 Y = Y,
 X1 = X)  
#Data = subset(Data,Anno>1) 

Data = Data[order(Data$Anno,Data$Cod_Provincia,
 decreasing=FALSE),] 
 ttimes= seq(ISOdate(2000,1,1),
  ISOdate(2000+(TT-1),1,1), "years") 
prova = spacetime::STFDF(spat,ttimes,Data) 
prova = prova[,(TT-T0+1):TT]
prova$Anno = prova$Anno - (startingT-1)
 out = list(prova,matw)
 return(out)
}

