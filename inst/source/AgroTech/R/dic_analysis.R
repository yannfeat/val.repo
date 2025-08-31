#' @import ggplot2
#' @import stats
#' @importFrom crayon green
#' @importFrom crayon bold
#' @importFrom crayon italic
#' @importFrom crayon red
#' @importFrom crayon blue
#' @importFrom crayon black
#' @importFrom nortest lillie.test
#' @importFrom nortest ad.test
#' @importFrom nortest cvm.test
#' @importFrom nortest pearson.test
#' @importFrom nortest sf.test
#' @importFrom utils read.table
#' @importFrom lmtest dwtest
#' @importFrom stats cor.test

dic.analysis <- function(trat,
                response,
                norm="sw",
                homog="bt",
                alpha.f=0.05,
                alpha.t=0.05,
                quali=TRUE,
                mcomp="tukey",
                grau=1,
                transf=1,
                constant=0,
                test="parametric",
                p.adj="holm",
                geom="bar",
                theme=theme_classic(),
                ylab="Response",
                sup=NA,
                CV=TRUE,
                xlab="",
                fill="lightblue",
                angle=0,
                family="sans",
                textsize=12,
                labelsize=4,
                dec=3,
                addmean=TRUE,
                errorbar=TRUE,
                posi="top",
                point="mean_sd",
                angle.label=0){
  mean.stat <-
    function (y, x, stat = "mean")
    {k<-0
    numerico<- NULL
    if(is.null(ncol(x))){
      if(is.numeric(x)){ k<-1
      numerico[1]<-1}}
    else{
      ncolx<-ncol(x)
      for (i in 1:ncolx) {
        if(is.numeric(x[,i])){
          k<-k+1
          numerico[k]<-i
        }}}
    cx <- deparse(substitute(x))
    cy <- deparse(substitute(y))
    x <- data.frame(c1 = 1, x)
    y <- data.frame(v1 = 1, y)
    nx <- ncol(x)
    ny <- ncol(y)
    namex <- names(x)
    namey <- names(y)
    if (nx == 2)
      namex <- c("c1", cx)
    if (ny == 2)
      namey <- c("v1", cy)
    namexy <- c(namex, namey)
    for (i in 1:nx) {
      x[, i] <- as.character(x[, i])}
    z <- NULL
    for (i in 1:nx){z <- paste(z, x[, i], sep = "&")}
    w <- NULL
    for (i in 1:ny) {
      m <- tapply(y[, i], z, stat)
      m <- as.matrix(m)
      w <- cbind(w, m)}
    nw <- nrow(w)
    c <- rownames(w)
    v <- rep("", nw * nx)
    dim(v) <- c(nw, nx)
    for (i in 1:nw) {
      for (j in 1:nx) {
        v[i, j] <- strsplit(c[i], "&")[[1]][j + 1]}}
    rownames(w) <- NULL
    junto <- data.frame(v[, -1], w)
    junto <- junto[, -nx]
    names(junto) <- namexy[c(-1, -(nx + 1))]
    if(k==1 & nx==2) {
      junto[,numerico[1]]<-as.character(junto[,numerico[1]])
      junto[,numerico[1]]<-as.numeric(junto[,numerico[1]])
      junto<-junto[order(junto[,1]),]}
    if (k>0 & nx > 2) {
      for (i in 1:k){
        junto[,numerico[i]]<-as.character(junto[,numerico[i]])
        junto[,numerico[i]]<-as.numeric(junto[,numerico[i]])}
      junto<-junto[do.call("order", c(junto[,1:(nx-1)])),]}
    rownames(junto)<-1:(nrow(junto))
    return(junto)}
  regression=function(trat,
                      resp,
                      ylab="Response",
                      xlab="Independent",
                      yname.poly="y",
                      xname.poly="x",
                      grau=NA,
                      theme=theme_classic(),
                      point="mean_sd",
                      color="gray80",
                      posi="top",
                      textsize=12,
                      se=FALSE,
                      ylim=NA,
                      family="sans",
                      pointsize=4.5,
                      linesize=0.8,
                      width.bar=NA,
                      n=NA,
                      SSq=NA,
                      DFres=NA){
    requireNamespace("ggplot2")
    if(is.na(width.bar)==TRUE){width.bar=0.1*mean(trat)}
    if(is.na(grau)==TRUE){grau=1}
    dados=data.frame(trat,resp)
    medias=c()
    dose=tapply(trat, trat, mean, na.rm=TRUE)
    mod=c()
    mod1=c()
    mod2=c()
    modm=c()
    mod1m=c()
    mod2m=c()
    text1=c()
    text2=c()
    text3=c()
    mods=c()
    mod1s=c()
    mod2s=c()
    fparcial1=c()
    fparcial2=c()
    fparcial3=c()
    media=tapply(resp, trat, mean, na.rm=TRUE)
    desvio=tapply(resp, trat, sd, na.rm=TRUE)
    erro=tapply(resp, trat, sd, na.rm=TRUE)/sqrt(table(trat))
    dose=tapply(trat, trat, mean, na.rm=TRUE)
    moda=lm(resp~trat)
    mod1a=lm(resp~trat+I(trat^2))
    mod2a=lm(resp~trat+I(trat^2)+I(trat^3))
    mods=summary(moda)$coefficients
    mod1s=summary(mod1a)$coefficients
    mod2s=summary(mod2a)$coefficients
    modm=lm(media~dose)
    mod1m=lm(media~dose+I(dose^2))
    mod2m=lm(media~dose+I(dose^2)+I(dose^3))

    modf1=lm(resp~trat)
    modf2=lm(resp~trat+I(trat^2))
    modf3=lm(resp~trat+I(trat^2)+I(trat^3))
    modf1ql=anova(modf1)
    modf2ql=anova(modf2)
    modf3ql=anova(modf3)
    modf1q=aov(resp~as.factor(trat))
    res=anova(modf1q)
    fadj1=anova(modf1,modf1q)[2,c(3,4,5,6)]
    fadj2=anova(modf2,modf1q)[2,c(3,4,5,6)]
    fadj3=anova(modf3,modf1q)[2,c(3,4,5,6)]
    if(is.na(DFres)==TRUE){DFres=res[2,1]}
    if(is.na(SSq)==TRUE){SSq=res[2,2]}
    df1=c(modf3ql[1,1],fadj1[1,1],DFres)
    df2=c(modf3ql[1:2,1],fadj2[1,1],DFres)
    df3=c(modf3ql[1:3,1],fadj3[1,1],DFres)
    sq1=c(modf3ql[1,2],fadj1[1,2],SSq)
    sq2=c(modf3ql[1:2,2],fadj2[1,2],SSq)
    sq3=c(modf3ql[1:3,2],fadj3[1,2],SSq)
    qm1=sq1/df1
    qm2=sq2/df2
    qm3=sq3/df3

    if(grau=="1"){fa1=data.frame(cbind(df1,sq1,qm1))
    fa1$f1=c(fa1$qm1[1:2]/fa1$qm1[3],NA)
    fa1$p=c(pf(fa1$f1[1:2],fa1$df1[1:2],fa1$df1[3],lower.tail = F),NA)
    colnames(fa1)=c("Df","SSq","MSQ","F","p-value");rownames(fa1)=c("Linear","Deviation","Residual")}

    if(grau=="2"){fa2=data.frame(cbind(df2,sq2,qm2))
    fa2$f2=c(fa2$qm2[1:3]/fa2$qm2[4],NA)
    fa2$p=c(pf(fa2$f2[1:3],fa2$df2[1:3],fa2$df2[4],lower.tail = F),NA)
    colnames(fa2)=c("Df","SSq","MSQ","F","p-value");rownames(fa2)=c("Linear","Quadratic","Deviation","Residual")}

    if(grau=="3"){
      fa3=data.frame(cbind(df3,sq3,qm3))
      fa3$f3=c(fa3$qm3[1:4]/fa3$qm3[5],NA)
      fa3$p=c(pf(fa3$f3[1:4],fa3$df3[1:4],fa3$df3[5],lower.tail = F),NA)
      colnames(fa3)=c("Df","SSq","MSQ","F","p-value");rownames(fa3)=c("Linear","Quadratic","Cubic","Deviation","Residual")}

    if(grau=="1"){r2=round(summary(modm)$r.squared, 2)}
    if(grau=="2"){r2=round(summary(mod1m)$r.squared, 2)}
    if(grau=="3"){r2=round(summary(mod2m)$r.squared, 2)}
    if(grau=="1"){
      if(is.na(n)==FALSE){coef1=round(coef(moda)[1],n)}else{coef1=coef(moda)[1]}
      if(is.na(n)==FALSE){coef2=round(coef(moda)[2],n)}else{coef2=coef(moda)[2]}
      s1=s <- sprintf("%s == %e %s %e*%s ~~~~~ italic(R^2) == %0.2f",
                      yname.poly,
                      coef1,
                      ifelse(coef2 >= 0, "+", "-"),
                      abs(coef2),
                      xname.poly,
                      r2)}
    if(grau=="2"){
      if(is.na(n)==FALSE){coef1=round(coef(mod1a)[1],n)}else{coef1=coef(mod1a)[1]}
      if(is.na(n)==FALSE){coef2=round(coef(mod1a)[2],n)}else{coef2=coef(mod1a)[2]}
      if(is.na(n)==FALSE){coef3=round(coef(mod1a)[3],n)}else{coef3=coef(mod1a)[3]}
      s2=s <- sprintf("%s == %e %s %e * %s %s %e * %s^2 ~~~~~ italic(R^2) ==  %0.2f",
                      yname.poly,
                      coef1,
                      ifelse(coef2 >= 0, "+", "-"),
                      abs(coef2),
                      xname.poly,
                      ifelse(coef3 >= 0, "+", "-"),
                      abs(coef3),
                      xname.poly,
                      r2)}
    if(grau=="3"){
      if(is.na(n)==FALSE){coef1=round(coef(mod2a)[1],n)}else{coef1=coef(mod2a)[1]}
      if(is.na(n)==FALSE){coef2=round(coef(mod2a)[2],n)}else{coef2=coef(mod2a)[2]}
      if(is.na(n)==FALSE){coef3=round(coef(mod2a)[3],n)}else{coef3=coef(mod2a)[3]}
      if(is.na(n)==FALSE){coef4=round(coef(mod2a)[4],n)}else{coef4=coef(mod2a)[4]}
      s3=s <- sprintf("%s == %e %s %e * %s %s %e * %s^2 %s %0.e * %s^3 ~~~~~ italic(R^2) == %0.2f",
                      yname.poly,
                      coef1,
                      ifelse(coef2 >= 0, "+", "-"),
                      abs(coef2),
                      xname.poly,
                      ifelse(coef3 >= 0, "+", "-"),
                      abs(coef3),
                      xname.poly,
                      ifelse(coef4 >= 0, "+", "-"),
                      abs(coef4),
                      xname.poly,
                      r2)}
    data1=data.frame(trat,resp)
    data1=data.frame(trat=dose,#as.numeric(as.character(names(media))),
                     resp=media,
                     desvio, erro)
    grafico=ggplot(data1,aes(x=trat,y=resp))
    if(point=="all"){grafico=grafico+
      geom_point(data=dados,
                 aes(y=resp,x=trat),shape=21,
                 fill=color,color="black")}
    if(point=="mean_sd"){grafico=grafico+
      geom_errorbar(aes(ymin=resp-desvio,ymax=resp+desvio),width=width.bar,size=linesize)}
    if(point=="mean_se"){grafico=grafico+
      geom_errorbar(aes(ymin=resp-erro,ymax=resp+erro),width=width.bar,size=linesize)}
    if(point=="mean"){grafico=grafico}
    grafico=grafico+geom_point(aes(fill=as.factor(rep(1,length(resp)))),na.rm=TRUE,
                               size=pointsize,shape=21,
                               color="black")+
      theme+ylab(ylab)+xlab(xlab)
    if(is.na(ylim[1])==TRUE){grafico=grafico}else{grafico=grafico+ylim(ylim)}

    if(grau=="0"){grafico=grafico+geom_line(y=mean(resp),size=linesize,lty=2)}
    if(grau=="1"){grafico=grafico+geom_smooth(method = "lm",se=se, na.rm=TRUE, formula = y~x,size=linesize,color="black")}
    if(grau=="2"){grafico=grafico+geom_smooth(method = "lm",se=se, na.rm=TRUE, formula = y~x+I(x^2),size=linesize,color="black")}
    if(grau=="3"){grafico=grafico+geom_smooth(method = "lm",se=se, na.rm=TRUE, formula = y~x+I(x^2)+I(x^3),size=linesize,color="black")}
    if(grau=="0"){grafico=grafico+
      scale_fill_manual(values=color,label=paste("y =",round(mean(resp),3)),name="")}
    if(grau=="1"){grafico=grafico+
      scale_fill_manual(values=color,label=c(parse(text=s1)),name="")}
    if(grau=="2"){grafico=grafico+
      scale_fill_manual(values=color,label=c(parse(text=s2)),name="")}
    if(grau=="3"){grafico=grafico+
      scale_fill_manual(values=color,label=c(parse(text=s3)),name="")}

    if(color=="gray"){if(grau=="1"){grafico=grafico+
      scale_fill_manual(values="black",label=c(parse(text=s1)),name="")}
      if(grau=="2"){grafico=grafico+
        scale_fill_manual(values="black",label=c(parse(text=s2)),name="")}
      if(grau=="3"){grafico=grafico+
        scale_fill_manual(values="black",label=c(parse(text=s3)),name="")}
    }

    grafico=grafico+
      theme(text = element_text(size=textsize,color="black",family=family),
            axis.text = element_text(size=textsize,color="black",family=family),
            axis.title = element_text(size=textsize,color="black",family=family),
            legend.position = posi,
            legend.text=element_text(size=textsize),
            legend.direction = "vertical",
            legend.text.align = 0,
            legend.justification = 0)
    print(grafico)
    if(grau==1){
      cat("\n----------------------------------------------------\n")
      cat("Regression Models")
      cat("\n----------------------------------------------------\n")
      print(mods)
      cat("\n----------------------------------------------------\n")
      cat("Deviations from regression")
      cat("\n----------------------------------------------------\n")
      print(as.matrix(fa1),na.print=" ")
    }
    if(grau==2){
      cat("\n----------------------------------------------------\n")
      cat("Regression Models")
      cat("\n----------------------------------------------------\n")
      print(mod1s)
      cat("\n----------------------------------------------------\n")
      cat("Deviations from regression")
      cat("\n----------------------------------------------------\n")
      print(as.matrix(fa2),na.print=" ")
    }
    if(grau==3){
      cat("\n----------------------------------------------------\n")
      cat("Regression Models")
      cat("\n----------------------------------------------------\n")
      print(mod2s)
      cat("\n----------------------------------------------------\n")
      cat("Deviations from regression")
      cat("\n----------------------------------------------------\n")
      print(as.matrix(fa3),na.print=" ")
    }
    graficos=list(grafico)
  }

  levenehomog <- function (y, ...) {
    UseMethod("levenehomog")}

  levenehomog.default <- function (y, group, center=median, ...) {
    if (!is.numeric(y))
      stop(deparse(substitute(y)), " is not a numeric variable")
    if (!is.factor(group)){warning(deparse(substitute(group)), " coerced to factor.")
      group <- as.factor(group)}
    valid <- complete.cases(y, group)
    meds <- tapply(y[valid], group[valid], center, ...)
    resp <- abs(y - meds[group])
    table <- anova(lm(resp ~ group))[, c(1, 4, 5)]
    rownames(table)[2] <- " "
    dots <- deparse(substitute(...))
    attr(table, "heading") <- paste("Levene's Test  (center = ",
                                    deparse(substitute(center)),
                                    if(!(dots == "NULL")) paste(":", dots),  ")", sep="")
    table}


  levenehomog.formula <- function(y, data, ...) {
    form <- y
    mf <- if (missing(data)) model.frame(form) else model.frame(form, data)
    if (any(sapply(2:dim(mf)[2], function(j) is.numeric(mf[[j]]))))
      stop("Levene's test is not appropriate with quantitative explanatory variables.")
    y <- mf[,1]
    if(dim(mf)[2]==2) group <- mf[,2]
    else {
      if (length(grep("\\+ | \\| | \\^ | \\:",form))>0) stop("Model must be completely crossed formula only.")
      group <- interaction(mf[,2:dim(mf)[2]])}
    levenehomog.default(y=y, group=group, ...)}

  levenehomog.lm <- function(y, ...) {
    m <- model.frame(y)
    m$..y <- model.response(m)
    f <- formula(y)
    f[2] <- expression(..y)
    levenehomog.formula(f, data=m, ...)}

  ordenacao=function (treatment, means, alpha, pvalue, console){
    n <- length(means)
    z <- data.frame(treatment, means)
    letras<-c(letters[1:26],LETTERS[1:26],1:9,
              c(".","+","-","*","/","#","$","%","&","^","[","]",":",
                "@",";","_","?","!","=","#",rep(" ",2000)))
    w <- z[order(z[, 2], decreasing = TRUE), ]
    M<-rep("",n)
    k<-1
    k1<-0
    j<-1
    i<-1
    cambio<-n
    cambio1<-0
    chequeo=0
    M[1]<-letras[k]
    q <- as.numeric(rownames(w)) #Check
    while(j<n) {
      chequeo<-chequeo+1
      if (chequeo > n) break
      for(i in j:n) {
        s<-pvalue[q[i],q[j]]>alpha
        if(s) {
          if(lastC(M[i]) != letras[k])M[i]<-paste(M[i],letras[k],sep="")
        }
        else {
          k<-k+1
          cambio<-i
          cambio1<-0
          ja<-j
          for(jj in cambio:n) M[jj]<-paste(M[jj],"",sep="") # El espacio
          M[cambio]<-paste(M[cambio],letras[k],sep="")
          for( v in ja:cambio) {
            if(pvalue[q[v],q[cambio]]<=alpha) {j<-j+1
            cambio1<-1
            }
            else break
          }
          break
        }
      }
      if (cambio1 ==0 )j<-j+1
    }
    w<-data.frame(w,stat=M)
    trt <- as.character(w$treatment)
    means <- as.numeric(w$means)
    output <- data.frame(means, groups=M)
    rownames(output)<-trt
    if(k>81)
      cat("\n",k,"groups are estimated.The number of groups exceeded the maximum of 81 labels. change to group=FALSE.\n")
    invisible(output)
  }
  lastC <-
    function(x) {
      y<-sub(" +$", "",x)
      p1<-nchar(y)
      cc<-substr(y,p1,p1)
      return(cc)}
  duncan <- function(y,
                     trt,
                     DFerror,
                     MSerror,
                     alpha = 0.05,
                     group = TRUE,
                     main = NULL,
                     console = FALSE)
  {name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  if(is.null(main))main<-paste(name.y,"~", name.t)
  clase<-c("aov","lm")
  if("aov"%in%class(y) | "lm"%in%class(y)){
    if(is.null(main))main<-y$call
    A<-y$model
    DFerror<-df.residual(y)
    MSerror<-deviance(y)/DFerror
    y<-A[,1]
    ipch<-pmatch(trt,names(A))
    nipch<- length(ipch)
    for(i in 1:nipch){
      if (is.na(ipch[i]))
        return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))}
    name.t<- names(A)[ipch][1]
    trt <- A[, ipch]
    if (nipch > 1){
      trt <- A[, ipch[1]]
      for(i in 2:nipch){
        name.t <- paste(name.t,names(A)[ipch][i],sep=":")
        trt <- paste(trt,A[,ipch[i]],sep=":")
      }}
    name.y <- names(A)[1]
  }
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  Mean<-mean(junto[,1])
  CV<-sqrt(MSerror)*100/Mean
  medians<-mean.stat(junto[,1],junto[,2],stat="median")
  for(i in c(1,5,2:4)) {
    x <- mean.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
    medians<-cbind(medians,x[,2])
  }
  medians<-medians[,3:7]
  names(medians)<-c("Min","Max","Q25","Q50","Q75")
  means <- mean.stat(junto[,1],junto[,2],stat="mean") # change
  sds <-   mean.stat(junto[,1],junto[,2],stat="sd") #change
  nn <-   mean.stat(junto[,1],junto[,2],stat="length") # change
  means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
  names(means)[1:2]<-c(name.t,name.y)
  ntr<-nrow(means)
  Tprob<-NULL
  k<-0
  for(i in 2:ntr){
    k<-k+1
    x <- suppressWarnings(warning(qtukey((1-alpha)^(i-1), i, DFerror)))
    if(x=="NaN")break
    else Tprob[k]<-x
  }
  if(k<(ntr-1)){
    for(i in k:(ntr-1)){
      f <- Vectorize(function(x)ptukey(x,i+1,DFerror)-(1-alpha)^i)
      Tprob[i]<-uniroot(f, c(0,100))$root
    }
  }
  Tprob<-as.numeric(Tprob)
  nr <- unique(nn[,2])
  if(console){
    cat("\nStudy:", main)
    cat("\n\nDuncan's new multiple range test\nfor",name.y,"\n")
    cat("\nMean Square Error: ",MSerror,"\n\n")
    cat(paste(name.t,",",sep="")," means\n\n")
    print(data.frame(row.names = means[,1], means[,2:6]))
  }
  if(length(nr) == 1 ) sdtdif <- sqrt(MSerror/nr)
  else {
    nr1 <-  1/mean(1/nn[,2])
    sdtdif <- sqrt(MSerror/nr1)
  }
  DUNCAN <- Tprob * sdtdif
  names(DUNCAN)<-2:ntr
  duncan<-data.frame(Table=Tprob,CriticalRange=DUNCAN)
  if ( group & length(nr) == 1 & console){
    cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
    cat("\nCritical Range\n")
    print(DUNCAN)}
  if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
  if ( length(nr) != 1) duncan<-NULL
  Omeans<-order(means[,2],decreasing = TRUE) #correccion 2019, 1 abril.
  Ordindex<-order(Omeans)
  comb <-utils::combn(ntr,2)
  nn<-ncol(comb)
  dif<-rep(0,nn)
  DIF<-dif
  LCL<-dif
  UCL<-dif
  pvalue<-dif
  odif<-dif
  sig<-NULL
  for (k in 1:nn) {
    i<-comb[1,k]
    j<-comb[2,k]
    dif[k]<-means[i,2]-means[j,2]
    DIF[k]<-abs(dif[k])
    nx<-abs(i-j)+1
    odif[k] <- abs(Ordindex[i]- Ordindex[j])+1
    pvalue[k]<- round(1-ptukey(DIF[k]/sdtdif,odif[k],DFerror)^(1/(odif[k]-1)),4)
    LCL[k] <- dif[k] - DUNCAN[odif[k]-1]
    UCL[k] <- dif[k] + DUNCAN[odif[k]-1]
    sig[k]<-" "
    if (pvalue[k] <= 0.001) sig[k]<-"***"
    else  if (pvalue[k] <= 0.01) sig[k]<-"**"
    else  if (pvalue[k] <= 0.05) sig[k]<-"*"
    else  if (pvalue[k] <= 0.1) sig[k]<-"."
  }
  if(!group){
    tr.i <- means[comb[1, ],1]
    tr.j <- means[comb[2, ],1]
    comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
    rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
    if(console){cat("\nComparison between treatments means\n\n")
      print(comparison)}
    groups=NULL
  }
  if (group) {
    comparison=NULL
    Q<-matrix(1,ncol=ntr,nrow=ntr)
    p<-pvalue
    k<-0
    for(i in 1:(ntr-1)){
      for(j in (i+1):ntr){
        k<-k+1
        Q[i,j]<-p[k]
        Q[j,i]<-p[k]
      }
    }
    groups <- ordenacao(means[, 1], means[, 2],alpha, Q,console)
    names(groups)[1]<-name.y
    if(console) {
      cat("\nMeans with the same letter are not significantly different.\n\n")
      print(groups)
    }
  }
  parameters<-data.frame(test="Duncan",name.t=name.t,ntr = ntr,alpha=alpha)
  statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
  rownames(parameters)<-" "
  rownames(statistics)<-" "
  rownames(means)<-means[,1]
  means<-means[,-1]
  output<-list(statistics=statistics,parameters=parameters, duncan=duncan,
               means=means,comparison=comparison,groups=groups)
  class(output)<-"group"
  invisible(output)
  }

  TUKEY <- function(y, trt, DFerror,
                    MSerror, alpha=0.05, group=TRUE,
                    main = NULL,unbalanced=FALSE,console=FALSE){
    name.y <- paste(deparse(substitute(y)))
    name.t <- paste(deparse(substitute(trt)))
    if(is.null(main))main<-paste(name.y,"~", name.t)
    clase<-c("aov","lm")
    if("aov"%in%class(y) | "lm"%in%class(y)){
      if(is.null(main))main<-y$call
      A<-y$model
      DFerror<-df.residual(y)
      MSerror<-deviance(y)/DFerror
      y<-A[,1]
      ipch<-pmatch(trt,names(A))
      nipch<- length(ipch)
      for(i in 1:nipch){
        if (is.na(ipch[i]))
          return(if(console)cat("Name: ", trt, "\n", names(A)[-1], "\n"))
      }
      name.t<- names(A)[ipch][1]
      trt <- A[, ipch]
      if (nipch > 1){
        trt <- A[, ipch[1]]
        for(i in 2:nipch){
          name.t <- paste(name.t,names(A)[ipch][i],sep=":")
          trt <- paste(trt,A[,ipch[i]],sep=":")
        }}
      name.y <- names(A)[1]
    }
    junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
    Mean<-mean(junto[,1])
    CV<-sqrt(MSerror)*100/Mean
    medians<-mean.stat(junto[,1],junto[,2],stat="median")
    for(i in c(1,5,2:4)) {
      x <- mean.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
      medians<-cbind(medians,x[,2])
    }
    medians<-medians[,3:7]
    names(medians)<-c("Min","Max","Q25","Q50","Q75")
    means <- mean.stat(junto[,1],junto[,2],stat="mean")
    sds <-   mean.stat(junto[,1],junto[,2],stat="sd")
    nn <-   mean.stat(junto[,1],junto[,2],stat="length")
    means<-data.frame(means,std=sds[,2],r=nn[,2],medians)
    names(means)[1:2]<-c(name.t,name.y)
    ntr<-nrow(means)
    Tprob <- qtukey(1-alpha,ntr, DFerror)
    nr<-unique(nn[, 2])
    nr1<-1/mean(1/nn[,2])
    if(console){
      cat("\nStudy:", main)
      cat("\n\nHSD Test for",name.y,"\n")
      cat("\nMean Square Error: ",MSerror,"\n\n")
      cat(paste(name.t,",",sep="")," means\n\n")
      print(data.frame(row.names = means[,1], means[,2:6]))
      cat("\nAlpha:",alpha,"; DF Error:",DFerror,"\n")
      cat("Critical Value of Studentized Range:", Tprob,"\n")
    }
    HSD <- Tprob * sqrt(MSerror/nr)
    statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV,MSD=HSD)
    if ( group & length(nr) == 1 & console) cat("\nMinimun Significant Difference:",HSD,"\n")
    if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of means differences and alpha level(",alpha,")\n")
    if ( length(nr) != 1) statistics<-data.frame(MSerror=MSerror,Df=DFerror,Mean=Mean,CV=CV)
    comb <-utils::combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    sig<-NULL
    LCL<-dif
    UCL<-dif
    pvalue<-rep(0,nn)
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      dif[k]<-means[i,2]-means[j,2]
      sdtdif<-sqrt(MSerror * 0.5*(1/means[i,4] + 1/means[j,4]))
      if(unbalanced)sdtdif<-sqrt(MSerror /nr1)
      pvalue[k]<- round(1-ptukey(abs(dif[k])/sdtdif,ntr,DFerror),4)
      LCL[k] <- dif[k] - Tprob*sdtdif
      UCL[k] <- dif[k] + Tprob*sdtdif
      sig[k]<-" "
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    if(!group){
      tr.i <- means[comb[1, ],1]
      tr.j <- means[comb[2, ],1]
      comparison<-data.frame("difference" = dif, pvalue=pvalue,"signif."=sig,LCL,UCL)
      rownames(comparison)<-paste(tr.i,tr.j,sep=" - ")
      if(console){cat("\nComparison between treatments means\n\n")
        print(comparison)}
      groups=NULL
    }
    if (group) {
      comparison=NULL
      Q<-matrix(1,ncol=ntr,nrow=ntr)
      p<-pvalue
      k<-0
      for(i in 1:(ntr-1)){
        for(j in (i+1):ntr){
          k<-k+1
          Q[i,j]<-p[k]
          Q[j,i]<-p[k]
        }
      }
      groups <- ordenacao(means[, 1], means[, 2],alpha, Q,console)
      names(groups)[1]<-name.y
      if(console) {
        cat("\nTreatments with the same letter are not significantly different.\n\n")
        print(groups)
      }
    }
    parameters<-data.frame(test="Tukey",name.t=name.t,ntr = ntr, StudentizedRange=Tprob,alpha=alpha)
    rownames(parameters)<-" "
    rownames(statistics)<-" "
    rownames(means)<-means[,1]
    means<-means[,-1]
    output<-list(statistics=statistics,parameters=parameters,
                 means=means,comparison=comparison,groups=groups)
    class(output)<-"group"
    invisible(output)
  }


  sk<-function(y,
               trt,
               DFerror,
               SSerror,
               alpha = 0.05,
               group = TRUE,
               main = NULL){
    sk <- function(medias,s2,dfr,prob){
      bo <- 0
      si2 <- s2
      defr <- dfr
      parou <- 1
      np <- length(medias) - 1
      for (i in 1:np){
        g1 <- medias[1:i]
        g2 <- medias[(i+1):length(medias)]
        B0 <- sum(g1)^2/length(g1) + sum(g2)^2/length(g2) - (sum(g1) + sum(g2))^2/length(c(g1,g2))
        if (B0 > bo)
        {bo <- B0
        parou <- i}
      }

      g1 <- medias[1:parou]
      g2 <- medias[(parou+1):length(medias)]
      teste <- c(g1,g2)
      sigm2 <- (sum(teste^2) - sum(teste)^2/length(teste) + defr*si2)/(length(teste) + defr)
      lamb <- pi*bo/(2*sigm2*(pi-2))
      v0 <- length(teste)/(pi-2)
      p <- pchisq(lamb,v0,lower.tail = FALSE)
      if (p < prob) {
        for (i in 1:length(g1)){
          cat(names(g1[i]),"\n",file="sk_groups",append=TRUE)}
        cat("*","\n",file="sk_groups",append=TRUE)}
      if (length(g1)>1){sk(g1,s2,dfr,prob)}
      if (length(g2)>1){sk(g2,s2,dfr,prob)}
    }
    trt=factor(trt,unique(trt))
    trt1=trt
    levels(trt)=paste("T",1:length(levels(trt)),sep = "")
    medias <- sort(tapply(y,trt,mean),decreasing=TRUE)
    dfr <- DFerror

    rep <- tapply(y,trt,length)
    s0 <- MSerror <-SSerror/DFerror
    s2 <- s0/rep[1]
    prob <- alpha
    sk(medias,s2,dfr,prob)
    f <- names(medias)
    names(medias) <- 1:length(medias)
    resultado <- data.frame("r"=0,"f"=f,"m"=medias)
    if (file.exists("sk_groups") == FALSE) {stop} else{
      xx <- read.table("sk_groups")
      file.remove("sk_groups")
      x <- xx[[1]]
      x <- as.vector(x)
      z <- 1

      for (j in 1:length(x)){
        if (x[j] == "*")	{z <- z+1}
        for (i in 1:length(resultado$f)){
          if (resultado$f[i]==x[j]){
            resultado$r[i] <- z;}
        }
      }

    }
    letras<-letters
    if(length(resultado$r)>26) {
      l<-floor(length(resultado$r)/26)
      for(i in 1:l) letras<-c(letras,paste(letters,i,sep=''))
    }
    res <- 1
    for (i in 1:(length(resultado$r)-1))
    {
      if (resultado$r[i] != resultado$r[i+1]){
        resultado$r[i] <- letras[res]
        res <- res+1
        if (i == (length(resultado$r)-1)){
          resultado$r[i+1] <- letras[res]}
      }
      else{
        resultado$r[i] <- letras[res]
        if (i == (length(resultado$r)-1)){
          resultado$r[i+1] <- letras[res]
        }
      }
    }
    names(resultado) <- c("groups","Tratamentos","Means")
    resultado1=resultado[,c(3,1)]
    rownames(resultado1)=resultado$Tratamentos
    final=list(resultado1)[[1]]
    final=final[as.character(unique(trt)),]
    rownames(final)=as.character(unique(trt1))
    final
  }
  scottknott=function(means, df1, QME, nrep, alpha=0.05){
    sk1=function(means, df1, QME, nrep, alpha=alpha) {
      means=sort(means,decreasing=TRUE)
      n=1:(length(means)-1)
      n=as.list(n)
      f=function(n){list(means[c(1:n)],means[-c(1:n)])}
      g=lapply(n, f)
      b1=function(x){(sum(g[[x]][[1]])^2)/length(g[[x]][[1]]) +
          (sum(g[[x]][[2]])^2)/length(g[[x]][[2]])-
          (sum(c(g[[x]][[1]],g[[x]][[2]]))^2)/length(c(g[[x]][[1]],g[[x]][[2]]))}
      p=1:length(g)
      values=sapply(p,b1)
      minimo=min(values); maximo=max(values)
      alfa=(1/(length(means)+df1))*(sum((means-mean(means))^2)+(df1*QME/nrep))
      lambda=(pi/(2*(pi-2)))*(maximo/alfa)
      vq=qchisq((alpha),lower.tail=FALSE, df=length(means)/(pi-2))
      ll=1:length(values); da=data.frame(ll,values); da=da[order(-values),]
      ran=da$ll[1]
      r=g[[ran]]; r=as.list(r)
      i=ifelse(vq>lambda|length(means)==1, 1,2)
      means=list(means)
      res=list(means, r)
      return(res[[i]])
    }
    u=sk1(means, df1, QME, nrep, alpha=alpha)
    u=lapply(u, sk1, df1=df1, QME=QME, nrep=nrep, alpha=alpha)
    sk2=function(u){
      v1=function(...){c(u[[1]])}
      v2=function(...){c(u[[1]],u[[2]])}
      v3=function(...){c(u[[1]],u[[2]],u[[3]])}
      v4=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]])}
      v5=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]])}
      v6=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]])}
      v7=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]])}
      v8=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]],u[[8]])}
      v9=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]],u[[8]],u[[9]])}
      v10=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]],u[[8]],u[[9]],u[[10]])}
      lv=list(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
      l=length(u)
      ti=lv[[l]]
      u=ti()
      u=lapply(u, sk1, df1=df1, QME=QME, nrep=nrep, alpha=alpha)
      return(u)
    }
    u=sk2(u);u=sk2(u);u=sk2(u);u=sk2(u);u=sk2(u)
    u=sk2(u);u=sk2(u);u=sk2(u);u=sk2(u);u=sk2(u)
    v1=function(...){c(u[[1]])}
    v2=function(...){c(u[[1]],u[[2]])}
    v3=function(...){c(u[[1]],u[[2]],u[[3]])}
    v4=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]])}
    v5=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]])}
    v6=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]])}
    v7=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]])}
    v8=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]],u[[8]])}
    v9=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]],u[[8]],u[[9]])}
    v10=function(...){c(u[[1]],u[[2]],u[[3]],u[[4]],u[[5]],u[[6]],u[[7]],u[[8]],u[[9]],u[[10]])}
    lv=list(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)
    l=length(u)
    ti=lv[[l]]
    u=ti()
    rp=u
    l2=lapply(rp, length)
    l2=unlist(l2)
    rp2=rep(letters[1:length(rp)], l2)
    return(rp2)}
  if(is.na(sup==TRUE)){sup=0.1*mean(response)}
  if(angle.label==0){hjust=0.5}else{hjust=0}
  requireNamespace("nortest")
  requireNamespace("crayon")
  requireNamespace("ggplot2")

  if(test=="parametric"){
  if(transf==1){resp=response+constant}else{resp=((response+constant)^transf-1)/transf}
  if(transf==0){resp=log(response+constant)}
  if(transf==0.5){resp=sqrt(response+constant)}
  if(transf==-0.5){resp=1/sqrt(response+constant)}
  if(transf==-1){resp=1/(response+constant)}
  trat1=trat
  trat=as.factor(trat)
  a = anova(aov(resp ~ trat))
  aa = summary(aov(resp ~ trat))
  b = aov(resp ~ trat)
  anava=a
  colnames(anava)=c("GL","SQ","QM","Fcal","p-value")
  respad=b$residuals/sqrt(a$`Mean Sq`[2])
  out=respad[respad>3 | respad<(-3)]
  out=names(out)
  out=if(length(out)==0)("No discrepant point")else{out}
  if(norm=="sw"){norm1 = shapiro.test(b$res)}
  if(norm=="li"){norm1=nortest::lillie.test(b$residuals)}
  if(norm=="ad"){norm1=nortest::ad.test(b$residuals)}
  if(norm=="cvm"){norm1=nortest::cvm.test(b$residuals)}
  if(norm=="pearson"){norm1=nortest::pearson.test(b$residuals)}
  if(norm=="sf"){norm1=nortest::sf.test(b$residuals)}
  if(homog=="bt"){
    homog1 = bartlett.test(b$res ~ trat)
    statistic=homog1$statistic
    phomog=homog1$p.value
    method=paste("Bartlett test","(",names(statistic),")",sep="")
  }
  if(homog=="levene"){
    homog1 = levenehomog(b$res~trat)[1,]
    statistic=homog1$`F value`[1]
    phomog=homog1$`Pr(>F)`[1]
    method="Levene's Test (center = median)(F)"
    names(homog1)=c("Df", "F value","p.value")}
  indep = dwtest(b)
  resids=b$residuals/sqrt(a$`Mean Sq`[2])
  Ids=ifelse(resids>3 | resids<(-3), "darkblue","black")
  residplot=ggplot(data=data.frame(resids,Ids),aes(y=resids,x=1:length(resids)))+
    geom_point(shape=21,color="gray",fill="gray",size=3)+
    labs(x="",y="Standardized residuals")+
    geom_text(x=1:length(resids),label=1:length(resids),color=Ids,size=4)+
    scale_x_continuous(breaks=1:length(resids))+
    theme_classic()+theme(axis.text.y = element_text(size=12),
                          axis.text.x = element_blank())+
    geom_hline(yintercept = c(0,-3,3),lty=c(1,2,2),color="red",size=1)
  print(residplot)
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(bold("Normality of errors")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  normal=data.frame(Method=paste(norm1$method,"(",names(norm1$statistic),")",sep=""),
                    Statistic=norm1$statistic,
                    "p-value"=norm1$p.value)
  rownames(normal)=""
  print(normal)
  cat("\n")
  message(if(norm1$p.value>0.05){
    black("As the calculated p-value is greater than the 5% significance level, hypothesis H0 is not rejected. Therefore, errors can be considered normal")}
      else {"As the calculated p-value is less than the 5% significance level, H0 is rejected. Therefore, errors do not follow a normal distribution"})
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(bold("Homogeneity of Variances")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  homoge=data.frame(Method=method,
                    Statistic=statistic,
                    "p-value"=phomog)
  rownames(homoge)=""
  print(homoge)
  cat("\n")
  message(if(homog1$p.value>0.05){
    black("As the calculated p-value is greater than the 5% significance level,hypothesis H0 is not rejected. Therefore, the variances can be considered homogeneous")}
      else {"As the calculated p-value is less than the 5% significance level, H0 is rejected.Therefore, the variances are not homogeneous"})
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(bold("Independence from errors")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  indepe=data.frame(Method=paste(indep$method,"(",
                                 names(indep$statistic),")",sep=""),
                    Statistic=indep$statistic,
                    "p-value"=indep$p.value)
  rownames(indepe)=""
  print(indepe)
  cat("\n")
  message(if(indep$p.value>0.05){
    black("As the calculated p-value is greater than the 5% significance level, hypothesis H0 is not rejected. Therefore, errors can be considered independent")}
      else {"As the calculated p-value is less than the 5% significance level, H0 is rejected.Therefore, errors are not independent"})
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(bold("Additional Information")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(paste("\nCV (%) = ",round(sqrt(a$`Mean Sq`[2])/mean(resp,na.rm=TRUE)*100,2)))
  cat(paste("\nR-squared = ",round(a$`Mean Sq`[1]/(a$`Mean Sq`[2]+a$`Mean Sq`[1]),2)))
  cat(paste("\nMean = ",round(mean(response,na.rm=TRUE),4)))
  cat(paste("\nMedian = ",round(median(response,na.rm=TRUE),4)))
  cat("\nPossible outliers = ", out)
  cat("\n")
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(bold("Analysis of Variance")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  anava1=as.matrix(data.frame(anava))
  colnames(anava1)=c("Df","Sum Sq","Mean.Sq","F value","Pr(F)" )
  print(anava1,na.print = "")
  cat("\n\n")
  message(if (a$`Pr(>F)`[1]<alpha.f){
    black("As the calculated p-value, it is less than the 5% significance level.The hypothesis H0 of equality of means is rejected. Therefore, at least two treatments differ")}
      else {"As the calculated p-value is greater than the 5% significance level, H0 is not rejected"})
  cat(green(bold("\n\n-----------------------------------------------------------------\n")))
  if(quali==TRUE){cat(green(bold("Multiple Comparison Test")))}else{cat(green(bold("Regression")))}
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  if(quali==TRUE){
  if(mcomp=="tukey"){
    letra <- TUKEY(b, "trat", alpha=alpha.t)
    letra1 <- letra$groups; colnames(letra1)=c("resp","groups")}
  if(mcomp=="sk"){
    # letra=SK(b,"trat",sig.level=alpha.t)
    # letra1=data.frame(resp=letra$m.inf[,1],groups=letters[letra$groups])
    nrep=table(trat)[1]
    medias=sort(tapply(resp,trat,mean),decreasing = TRUE)
    letra=scottknott(means = medias,
             df1 = a$Df[2],
             nrep = nrep,
             QME = a$`Mean Sq`[2],
             alpha = alpha.t)
    letra1=data.frame(resp=medias,groups=letra)}
  if(mcomp=="duncan"){
    letra <- duncan(b, "trat", alpha=alpha.t)
    letra1 <- letra$groups; colnames(letra1)=c("resp","groups")}
  media = tapply(response, trat, mean, na.rm=TRUE)
  if(transf=="1"){letra1}else{letra1$respO=media[rownames(letra1)]}
  print(if(a$`Pr(>F)`[1]<alpha.f){letra1}else{"H0 is not rejected"})
  cat("\n")
  message(if(transf=="1"){}else{blue("\nNOTE: resp = transformed means; respO = averages without transforming\n")})
  if(transf==1 && norm1$p.value<0.05 | transf==1 && indep$p.value<0.05 | transf==1 &&homog1$p.value<0.05){
    message("\nYour analysis is not valid, suggests using a non-parametric test and try to transform the data")
    }
  else{}
  if(transf != 1 && norm1$p.value<0.05 | transf!=1 && indep$p.value<0.05 | transf!=1 && homog1$p.value<0.05){cat(red("\nWarning!!! Your analysis is not valid, suggests using a non-parametric test"))}else{}
  if(point=="mean_sd"){
  dadosm=data.frame(letra1,
                    media=tapply(response, trat, mean, na.rm=TRUE)[rownames(letra1)],
                    desvio=tapply(response, trat, sd, na.rm=TRUE)[rownames(letra1)])}
  if(point=="mean_se"){
  dadosm=data.frame(letra1,
                    media=tapply(response, trat, mean, na.rm=TRUE)[rownames(letra1)],
                    desvio=tapply(response, trat, sd, na.rm=TRUE)/sqrt(tapply(response, trat, length))[rownames(letra1)])}

  dadosm$trats=factor(rownames(dadosm),levels = unique(trat))
  dadosm$limite=dadosm$media+dadosm$desvio
  dadosm=dadosm[unique(as.character(trat)),]
  if(addmean==TRUE){dadosm$letra=paste(format(dadosm$media,digits = dec),dadosm$groups)}
  if(addmean==FALSE){dadosm$letra=dadosm$groups}
  trats=dadosm$trats
  limite=dadosm$limite
  media=dadosm$media
  desvio=dadosm$desvio
  letra=dadosm$letra
  if(geom=="bar"){grafico=ggplot(dadosm,aes(x=trats,y=media))
    if(fill=="trat"){grafico=grafico+
      geom_col(aes(fill=trats),color=1)}else{grafico=grafico+
      geom_col(aes(fill=trats),fill=fill,color=1)}
  if(errorbar==TRUE){grafico=grafico+
    geom_text(aes(y=media+sup+if(sup<0){-desvio}else{desvio},
                  label=letra),family=family,angle=angle.label,size=labelsize, hjust=hjust)}
  if(errorbar==FALSE){grafico=grafico+
    geom_text(aes(y=media+sup,label=letra),family=family,size=labelsize,angle=angle.label, hjust=hjust)}
  if(errorbar==TRUE){grafico=grafico+
    geom_errorbar(data=dadosm,aes(ymin=media-desvio,
                                  ymax=media+desvio,color=1),
                  color="black",width=0.3)}}
  if(geom=="point"){grafico=ggplot(dadosm,aes(x=trats,
                                              y=media))
  if(errorbar==TRUE){grafico=grafico+
    geom_text(aes(y=media+sup+if(sup<0){-desvio}else{desvio},
                  label=letra),family=family,angle=angle.label,size=labelsize, hjust=hjust)}
  if(errorbar==FALSE){grafico=grafico+
    geom_text(aes(y=media+sup,
                  label=letra),family=family,angle=angle.label, size=labelsize,hjust=hjust)}
  if(errorbar==TRUE){grafico=grafico+
    geom_errorbar(data=dadosm,
                  aes(ymin=media-desvio,
                      ymax=media+desvio,color=1),
                  color="black",width=0.3)}
  if(fill=="trat"){grafico=grafico+
    geom_point(aes(color=trats),size=5)}
  else{grafico=grafico+
    geom_point(aes(color=trats),
               color="black",
               fill=fill,shape=21,size=5)}}
  if(geom=="box"){
  datam1=data.frame(trats=factor(trat,levels = unique(as.character(trat))),
                    response)
  dadosm2=data.frame(letra1,
                     superior=tapply(response, trat, mean, na.rm=TRUE)[rownames(letra1)])
  dadosm2$trats=rownames(dadosm2)
  dadosm2=dadosm2[unique(as.character(trat)),]
  dadosm2$limite=dadosm$media+dadosm$desvio
  dadosm2$letra=paste(format(dadosm$media,digits = dec),dadosm$groups)
  trats=dadosm2$trats
  limite=dadosm2$limite
  superior=dadosm2$superior
  letra=dadosm2$letra
  stat_box=ggplot(datam1,aes(x=trats,y=response))+geom_boxplot()
  superior=ggplot_build(stat_box)$data[[1]]$ymax
  dadosm2$superior=superior+sup
  grafico=ggplot(datam1,aes(x=trats,y=response))
  if(fill=="trat"){grafico=grafico+geom_boxplot(aes(fill=trats))}
  else{grafico=grafico+
    geom_boxplot(aes(fill=trats),fill=fill)}
  grafico=grafico+
    geom_text(data=dadosm2,
              aes(y=superior,
                  label=letra),
              family = family,size=labelsize,angle=angle.label, hjust=hjust)}
  grafico=grafico+
    theme+
    ylab(ylab)+
    xlab(xlab)+
    theme(text = element_text(size=textsize,color="black", family = family),
          axis.text = element_text(size=textsize,color="black", family = family),
          axis.title = element_text(size=textsize,color="black", family = family),
          legend.position = "none")
  if(angle !=0){grafico=grafico+
    theme(axis.text.x=element_text(hjust = 1.01,angle = angle))}
  if(CV==TRUE){grafico=grafico+
    labs(caption=paste("p-value", if(a$`Pr(>F)`[1]<0.0001){paste("<", 0.0001)}
                                                  else{paste("=", round(a$`Pr(>F)`[1],4))},"; CV = ",
                                                  round(abs(sqrt(a$`Mean Sq`[2])/mean(resp))*100,2),"%"))}
  grafico=as.list(grafico)
  }
  if(quali==FALSE){
  trat=trat1
  # trat=as.numeric(as.character(trat))
  if(grau==1){graph=regression(trat,response, grau = 1,textsize=textsize,xlab=xlab,ylab=ylab, family=family,posi=posi,point=point)}
  if(grau==2){graph=regression(trat,response, grau = 2,textsize=textsize,xlab=xlab,ylab=ylab, family=family,posi=posi,point=point)}
  if(grau==3){graph=regression(trat,response, grau = 3,textsize=textsize,xlab=xlab,ylab=ylab, family=family,posi=posi,point=point)}
  grafico=graph[[1]]
  }}
  if(test=="noparametric"){
    kruskal=function (y,
                      trt,
                      alpha = 0.05,
                      p.adj = c("none", "holm",
                                "hommel", "hochberg", "bonferroni", "BH", "BY", "fdr"),
                      group = TRUE, main = NULL,console=FALSE){
      name.y <- paste(deparse(substitute(y)))
      name.t <- paste(deparse(substitute(trt)))
      if(is.null(main))main<-paste(name.y,"~", name.t)
      p.adj <- match.arg(p.adj)
      junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
      N <- nrow(junto)
      medians<-mean.stat(junto[,1],junto[,2],stat="median")
      for(i in c(1,5,2:4)) {
        x <- mean.stat(junto[,1],junto[,2],function(x)quantile(x)[i])
        medians<-cbind(medians,x[,2])}
      medians<-medians[,3:7]
      names(medians)<-c("Min","Max","Q25","Q50","Q75")
      Means <- mean.stat(junto[,1],junto[,2],stat="mean")
      sds <-   mean.stat(junto[,1],junto[,2], stat="sd")
      nn <-   mean.stat(junto[,1],junto[,2],stat="length")
      Means<-data.frame(Means,std=sds[,2],r=nn[,2],medians)
      rownames(Means)<-Means[,1]
      Means<-Means[,-1]
      names(Means)[1]<-name.y
      junto[, 1] <- rank(junto[, 1])
      means <- mean.stat(junto[, 1], junto[, 2], stat = "sum")
      sds <- mean.stat(junto[, 1], junto[, 2], stat = "sd")
      nn <- mean.stat(junto[, 1], junto[, 2], stat = "length")
      means <- data.frame(means, r = nn[, 2])
      names(means)[1:2] <- c(name.t, name.y)
      ntr <- nrow(means)
      nk <- choose(ntr, 2)
      DFerror <- N - ntr
      rs <- 0
      U <- 0
      for (i in 1:ntr) {
        rs <- rs + means[i, 2]^2/means[i, 3]
        U <- U + 1/means[i, 3]
      }
      S <- (sum(junto[, 1]^2) - (N * (N + 1)^2)/4)/(N - 1)
      H <- (rs - (N * (N + 1)^2)/4)/S
      p.chisq <- 1 - pchisq(H, ntr - 1)
      if(console){
        cat("\nStudy:", main)
        cat("\nKruskal-Wallis test's\nTies or no Ties\n")
        cat("\nCritical Value:", H)
        cat("\nDegrees of freedom:", ntr - 1)
        cat("\nPvalue Chisq  :", p.chisq, "\n\n")}
      DFerror <- N - ntr
      Tprob <- qt(1 - alpha/2, DFerror)
      MSerror <- S * ((N - 1 - H)/(N - ntr))
      means[, 2] <- means[, 2]/means[, 3]
      if(console){cat(paste(name.t, ",", sep = ""), " means of the ranks\n\n")
        print(data.frame(row.names = means[, 1], means[, -1]))
        cat("\nPost Hoc Analysis\n")}
      if (p.adj != "none") {
        if(console)cat("\nP value adjustment method:", p.adj)
        a <- 1e-06
        b <- 1
        for (i in 1:100) {
          x <- (b + a)/2
          xr <- rep(x, nk)
          d <- p.adjust(xr, p.adj)[1] - alpha
          ar <- rep(a, nk)
          fa <- p.adjust(ar, p.adj)[1] - alpha
          if (d * fa < 0)
            b <- x
          if (d * fa > 0)
            a <- x}
        Tprob <- qt(1 - x/2, DFerror)
      }
      nr <- unique(means[, 3])
      if (group & console){
        cat("\nt-Student:", Tprob)
        cat("\nAlpha    :", alpha)}
      if (length(nr) == 1)  LSD <- Tprob * sqrt(2 * MSerror/nr)
      statistics<-data.frame(Chisq=H,Df=ntr-1,p.chisq=p.chisq)
      if ( group & length(nr) == 1 & console) cat("\nMinimum Significant Difference:",LSD,"\n")
      if ( group & length(nr) != 1 & console) cat("\nGroups according to probability of treatment differences and alpha level.\n")
      if ( length(nr) == 1) statistics<-data.frame(statistics,t.value=Tprob,MSD=LSD)
      comb <- utils::combn(ntr, 2)
      nn <- ncol(comb)
      dif <- rep(0, nn)
      LCL <- dif
      UCL <- dif
      pvalue <- dif
      sdtdif <- dif
      for (k in 1:nn) {
        i <- comb[1, k]
        j <- comb[2, k]
        dif[k] <- means[i, 2] - means[j, 2]
        sdtdif[k] <- sqrt(MSerror * (1/means[i,3] + 1/means[j, 3]))
        pvalue[k] <- 2*(1 - pt(abs(dif[k])/sdtdif[k],DFerror))
      }
      if (p.adj != "none") pvalue <- p.adjust(pvalue, p.adj)
      pvalue <- round(pvalue,4)
      sig <- rep(" ", nn)
      for (k in 1:nn) {
        if (pvalue[k] <= 0.001)
          sig[k] <- "***"
        else if (pvalue[k] <= 0.01)
          sig[k] <- "**"
        else if (pvalue[k] <= 0.05)
          sig[k] <- "*"
        else if (pvalue[k] <= 0.1)
          sig[k] <- "."
      }
      tr.i <- means[comb[1, ], 1]
      tr.j <- means[comb[2, ], 1]
      LCL <- dif - Tprob * sdtdif
      UCL <- dif + Tprob * sdtdif
      comparison <- data.frame(Difference = dif, pvalue = pvalue, "Signif."=sig, LCL, UCL)
      if (p.adj !="bonferroni" & p.adj !="none"){
        comparison<-comparison[,1:3]
        statistics<-data.frame(Chisq=H,p.chisq=p.chisq)}
      rownames(comparison) <- paste(tr.i, tr.j, sep = " - ")
      if (!group) {
        groups<-NULL
        if(console){
          cat("\nComparison between treatments mean of the ranks.\n\n")
          print(comparison)
        }
      }
      if (group) {
        comparison=NULL
        Q<-matrix(1,ncol=ntr,nrow=ntr)
        p<-pvalue
        k<-0
        for(i in 1:(ntr-1)){
          for(j in (i+1):ntr){
            k<-k+1
            Q[i,j]<-p[k]
            Q[j,i]<-p[k]
          }
        }
        groups <- ordenacao(means[, 1], means[, 2],alpha, Q, console)
        names(groups)[1]<-name.y
        if(console) {
          cat("\nTreatments with the same letter are not significantly different.\n\n")
          print(groups)
        }
      }
      ranks=means
      Means<-data.frame(rank=ranks[,2],Means)
      Means<-Means[,c(2,1,3:9)]
      parameters<-data.frame(test="Kruskal-Wallis",p.ajusted=p.adj,name.t=name.t,ntr = ntr,alpha=alpha)
      rownames(parameters)<-" "
      rownames(statistics)<-" "
      output<-list(statistics=statistics,parameters=parameters,
                   means=Means,comparison=comparison,groups=groups)
      class(output)<-"group"
      invisible(output)
    }
    krusk=kruskal(response,trat,p.adj = p.adj,alpha=alpha.t)
    cat(green(bold("\n\n-----------------------------------------------------------------\n")))
    cat(green(italic("Statistics")))
    cat(green(bold("\n-----------------------------------------------------------------\n")))
    print(krusk$statistics)
    cat(green(bold("\n\n-----------------------------------------------------------------\n")))
    cat(green(italic("Parameters")))
    cat(green(bold("\n-----------------------------------------------------------------\n")))
    print(krusk$parameters)
    cat(green(bold("\n\n-----------------------------------------------------------------\n")))
    cat(green(italic("Multiple Comparison Test")))
    cat(green(bold("\n-----------------------------------------------------------------\n")))
    saida=cbind(krusk$means[,c(1,3)],krusk$groups[rownames(krusk$means),])
    colnames(saida)=c("Mean","SD","Rank","Groups")
    print(saida)
    dadosm=data.frame(krusk$means,krusk$groups[rownames(krusk$means),])
    dadosm$trats=factor(rownames(dadosm),levels = unique(trat))
    dadosm$media=tapply(response,trat,mean, na.rm=TRUE)[rownames(krusk$means)]
    if(point=="mean_sd"){dadosm$std=tapply(response,trat,sd, na.rm=TRUE)[rownames(krusk$means)]}
    if(point=="mean_se"){dadosm$std=tapply(response, trat, sd, na.rm=TRUE)/
      sqrt(tapply(response, trat, length))[rownames(krusk$means)]}
    if(addmean==TRUE){dadosm$letra=paste(format(dadosm$response,digits = dec),dadosm$groups)}
    if(addmean==FALSE){dadosm$letra=dadosm$groups}
    trats=dadosm$trats
    limite=dadosm$limite
    media=dadosm$media
    std=dadosm$std
    letra=dadosm$letra
    if(geom=="bar"){grafico=ggplot(dadosm,
                                   aes(x=trats,y=response))
      if(fill=="trat"){grafico=grafico+
        geom_col(aes(fill=trats),color=1)}
      else{grafico=grafico+
        geom_col(aes(fill=trats),fill=fill,color=1)}
    if(errorbar==TRUE){grafico=grafico+
      geom_text(aes(y=media+sup+if(sup<0){-std}else{std},
                    label=letra),family=family,size=labelsize,angle=angle.label, hjust=hjust)}
    if(errorbar==FALSE){grafico=grafico+
      geom_text(aes(y=media+sup,label=letra),size=labelsize,family=family,angle=angle.label, hjust=hjust)}
    if(errorbar==TRUE){grafico=grafico+
      geom_errorbar(data=dadosm,aes(ymin=response-std,
                                    ymax=response+std,
                                    color=1),
                    color="black",width=0.3)}}
    if(geom=="point"){grafico=ggplot(dadosm,
                                     aes(x=trats,
                                         y=response))
    if(errorbar==TRUE){grafico=grafico+
      geom_text(aes(y=media+sup+if(sup<0){-std}else{std},
                    label=letra),
                family=family,angle=angle.label,size=labelsize, hjust=hjust)}
    if(errorbar==FALSE){grafico=grafico+
      geom_text(aes(y=media+sup,
                    label=letra),
                family=family,angle=angle.label, size=labelsize,hjust=hjust)}
    if(errorbar==TRUE){grafico=grafico+
      geom_errorbar(data=dadosm,
                    aes(ymin=response-std,
                        ymax=response+std,
                        color=1),
                    color="black",width=0.3)}
    if(fill=="trat"){grafico=grafico+
      geom_point(aes(color=trats),size=5)}
    else{grafico=grafico+
      geom_point(aes(color=trats),
                 color="black",
                 fill=fill,shape=21,size=5)}}
    if(geom=="box"){
    datam1=data.frame(trats=factor(trat,levels = unique(as.character(trat))),response)
    dadosm2=data.frame(krusk$means)
    dadosm2$trats=rownames(dadosm2)
    dadosm2$limite=dadosm2$response+dadosm2$std
    dadosm2$letra=paste(format(dadosm2$response,digits = dec),
                        dadosm$groups)
    dadosm2=dadosm2[unique(as.character(trat)),]
    trats=dadosm2$trats
    limite=dadosm2$limite
    letra=dadosm2$letra
    stat_box=ggplot(datam1,aes(x=trats,y=response))+geom_boxplot()
    superior=ggplot_build(stat_box)$data[[1]]$ymax
    dadosm2$superior=superior+sup

    grafico=ggplot(datam1,
                   aes(x=trats,
                       y=response))
      if(fill=="trat"){grafico=grafico+
        geom_boxplot(aes(fill=1))}
    else{grafico=grafico+
      geom_boxplot(aes(fill=trats),fill=fill)}
    grafico=grafico+
      geom_text(data=dadosm2,
                aes(y=superior,
                    label=letra),
                family = family,angle=angle.label, size=labelsize,hjust=hjust)}
    grafico=grafico+theme+
      ylab(ylab)+
      xlab(xlab)+
      theme(text = element_text(size=textsize,color="black", family = family),
            axis.title = element_text(size=textsize,color="black", family = family),
            axis.text = element_text(size=textsize,color="black", family = family),
            legend.position = "none")
    if(angle !=0){grafico=grafico+theme(axis.text.x=element_text(hjust = 1.01,angle = angle))}
}
  if(quali==TRUE){print(grafico)}
  graficos=list(grafico)#[[1]]
}
