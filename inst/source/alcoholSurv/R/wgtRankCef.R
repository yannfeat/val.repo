wgtRankCef <-
function(y,colGroups=NULL,phi="u878",phifunc=NULL,gammas=1,
                     alternative="greater",trunc=1,seed=NULL,random=FALSE){
  if (!is.null(seed)) set.seed(seed)
  #
  #  Check input
  #
  stopifnot(is.data.frame(y)|is.matrix(y))
  stopifnot(is.logical(random))
  if (!is.null(colGroups)) stopifnot(length(colGroups)==(dim(y)[2]))
  else if (!is.null(colnames(y))) colGroups<-colnames(y)
  else colGroups<-1:(dim(y)[2])
  if (sum(colGroups==colGroups[1])>1)
    stop("First group in colGroups cannot contain more than one individual.")
  if (length(phi)==1) phi<-rep(phi,length(unique(colGroups))-1)
  J<-dim(y)[2]
  ugroups<-unique(colGroups)
  ugroups<-ugroups[2:length(ugroups)]
  ntests<-length(ugroups)
  if (length(gammas)==1) gammas<-rep(gammas,ntests)
  else stopifnot(length(gammas)==ntests)
  #
  #  Define two functions
  #
  #  First function is score.
  score <-
    function (y, rankRG=NULL, phi = "u868", phifunc = NULL)
    {
      #
      #  Check input
      #
      stopifnot(is.matrix(y) | is.data.frame(y))
      stopifnot(0 == sum(is.na(as.vector(y))))
      stopifnot(2==(dim(y)[2]))
      if (!is.null(rankRG)){
        stopifnot(is.vector(rankRG)&(length(rankRG)==(dim(y)[1])))
        stopifnot(all((rankRG>=0)&(rankRG<=1)))
      }
      if (is.null(phifunc)) {
        stopifnot(is.element(phi, c("u868", "u878", "u888", "quade",
                                    "u858","wilc","noether")))
      }
      multrnksU <- function(pk, m1 = 2, m2 = 2, m = 2) {
        n <- length(pk)
        q <- rep(0, n)
        q <- rep(0, n)
        for (l in m1:m2) {
          q <- q + (l * choose(m, l) * (pk^(l - 1)) * ((1 -
                                                          pk)^(m - l)))
        }
        q/max(q)
      }
      noether<-function(pk){1*(pk>=(2/3))}

      u858 <- function(pk) {
        multrnksU(pk, m1 = 5, m2 = 8, m = 8)
      }
      u868 <- function(pk) {
        multrnksU(pk, m1 = 6, m2 = 8, m = 8)
      }
      u878 <- function(pk) {
        multrnksU(pk, m1 = 7, m2 = 8, m = 8)
      }
      u888 <- function(pk) {
        multrnksU(pk, m1 = 8, m2 = 8, m = 8)
      }
      quade <- function(pk) {
        pk
      }
      wilc <- function(pk) {
        rep(1, length(pk))
      }
      if (is.null(phifunc)) {
        if (phi == "u868")
          phifunc <- u868
        else if (phi == "u878")
          phifunc <- u878
        else if (phi == "u888")
          phifunc <- u888
        else if (phi == "quade")
          phifunc <- quade
        else if (phi == "wilc")
          phifunc <- wilc
        else if (phi == "u858")
          phifunc <- u858
        else if (phi == "noether")
          phifunc <- noether
      }
      J <- dim(y)[2]
      nset <- dim(y)[1]
      mx <- apply(y, 1, max)
      mn <- apply(y, 1, min)


      if (sum(mx == mn) > 0) {
        who <- mx != mn
        y <- y[who, ]
        mn <- mn[who]
        mx <- mx[who]
        rankRG <- rankRG[who]
        #warning(paste(sum(1 - who), " blocks had 0 range and were removed."))
        nset <- dim(y)[1]
      }
      if (is.null(rankRG)) rkrg <- phifunc((rank(mx-mn)/(length(mn)+1)))
      else rkrg<-phifunc(rankRG)
      sc <- t(apply(y, 1, rank))
      for (j in 1:J) sc[, j] <- sc[, j] * rkrg
      sc
    }
  #
  #  Second function is wgtRankCprescored
  wgtRankCprescored<-
    function (y, gamma = 1, alternative = "greater")
    {
      # This version assumes that y has been prescored by another function
      #
      #  Check input
      #
      stopifnot((alternative == "greater") | (alternative == "less"))
      stopifnot(is.matrix(y) | is.data.frame(y))
      stopifnot((dim(y)[2])==2)
      stopifnot(0 == sum(is.na(as.vector(y))))
      stopifnot(is.vector(gamma) & (length(gamma) == 1))
      stopifnot(gamma >= 1)
      mx <- apply(y, 1, max)
      mn <- apply(y, 1, min)

      if (sum(mx == mn) > 0) {
        # Remove any blocks that are constant
        who <- mx != mn
        y <- y[who, ]
        mn <- mn[who]
        mx <- mx[who]
        y <- y[who]
        #warning(paste(sum(1 - who), " blocks had 0 range and were removed."))
        nset <- dim(y)[1]
      }
      #
      #  Compute P-value bound
      #
      if (alternative=="less") y<-(-y)
      df <- y[,1]-y[,2]
      ts<-sum(df)
      ex<-sum(abs(df))*(gamma-1)/(1+gamma)
      vr<-4*sum(df*df)*gamma/((1+gamma)^2)
      dev<-(ts-ex)/sqrt(vr)
      pval<-1-stats::pnorm(dev)
      list(pval=pval,ss=length(df),tmax=sum(df>0))
    }
  #  End of defined functions.  Return to main program.
  #
  #  Prepare empty container for final results
  pvals<-rep(NA,ntests)
  names(pvals)<-ugroups
  nbks<-rep(NA,ntests)
  names(nbks)<-ugroups
  tmax<-rep(NA,ntests)
  names(tmax)<-ugroups
  #
  #  Determine within and between block ranks
  #
  if (random) rk<-t(apply(y,1,rank,ties.method="random"))
  else rk<-t(apply(y,1,rank))  # This will remove blocks without unique extremes
  rg<-apply(y,1,max)-apply(y,1,min)
  rankrg<-rank(rg)/(length(rg)+1)
  #
  #   Main computation
  #
  who1<-(rk[,1]==J)|(rk[,1]==1)
  for (i in 1:ntests){
    cols<-which(colGroups==ugroups[i])
    whoi<-rep(FALSE,dim(y)[1])
    for (j in cols) whoi<-whoi|(rk[,j]==J)|(rk[,j]==1)
    who<-who1&whoi  # who is FALSE for a block without one J and one 1 rank
    if (sum(who)>1){
      yc<-y[who,c(1,cols)]
      if (2==(dim(yc)[2])) yi<-yc
      else {
        ycmx<-apply(yc[,2:(dim(yc)[2])],1,max)
        ycmn<-apply(yc[,2:(dim(yc)[2])],1,min)
        ytr<-yc[,1]
        yco<-ycmn
        yco[ytr<ycmx]<-ycmx[ytr<ycmx]
        yi<-cbind(ytr,yco)
      }
      if (phi[i]=="none") o<-wgtRankCprescored(yi,gamma=gammas[i],
                                            alternative = alternative)
      else {
        scrd<-score(yi,rankRG=rankrg[who],phi=phi[i],phifunc=phifunc)
        o<-wgtRankCprescored(scrd,gamma=gammas[i],alternative=alternative)
      }
      pvals[i]<-o$pval
      nbks[i]<-o$ss
      tmax[i]<-o$tmax
    }
    else {
      pvals[i]<-1
      nbks[i]<-sum(who)
    }
  }
  tprod<-sensitivitymv::truncatedP(pvals,trunc=trunc)
  list(tProd.pval=tprod,pvals=pvals,blocks.used=nbks,treated.max=tmax)
}
