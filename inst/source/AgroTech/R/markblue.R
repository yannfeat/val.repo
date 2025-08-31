#' Spray deposit (glowing blue marker)
#'
#' @description This is a function to determine spray deposit using bright blue marker and then after performing tests of assumptions, analysis of variance and comparison of means
#'
#' @param d1 Curved worksheet
#' @param d2 Experiment worksheet
#' @param vl Wash volume (mL)
#' @param ci Initial marker concentration
#' @param naf2 Sheet area (cm2)
#' @param ncu2 Column referring to the curve (\emph{default} is 1)
#' @param nresp2 Column referring to absorbance
#' @param ntrat2 Column referring to treatment
#' @param nrep2 Column referring to repetition
#' @param analysis Perform statistical analysis
#' @param design Experiment design
#' @param transf Data transformation
#' @param quali Qualitative or quantitative treatment (\emph{default} is TRUE)
#' @param grau degree of the polynomial (when treatment is quantitative)
#' @param test Parametric or Nonparametric (\emph{default} is "parametric")
#' @param mcomp Mean comparison test (\emph{default} is "tukey")
#' @param ylab y axis name (\emph{default} is expression(mu~cm^2))
#' @param save.xlsx Want to export in excel format (\emph{default} is FALSE)
#'
#' @note Curve name on the curve worksheet (d1) must be the same as the curve name on the experiment worksheet (d2)
#'
#' @return Returns the comparison between the treatments of the experiment
#'
#' @import ggplot2
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom stats na.omit
#' @importFrom stats coef
#' @importFrom readxl read_excel
#' @importFrom ggrepel geom_text_repel
#' @importFrom xlsx write.xlsx
#'
#' @author Rodrigo Yudi Palhaci Marubayashi, \email{marubayashi@uel.br}
#' @author Gabriel Danilo Shimizu
#' @author Otavio Jorge Grigoli Abi Saab
#'
#' @references
#' No reference
#'
#' @export
#'
#' @seealso \link{markmet}
#'
#' @examples
#' data("example_markbluecurve")
#' data("example_markblue")
#' markblue(d1=example_markbluecurve,
#'          d2=example_markblue,
#'          vl=20,
#'          ci=1500,
#'          ncu2 = 1,
#'          ntrat2 = 2,
#'          nrep2 = 3,
#'          nresp2 = 4,
#'          naf2 = 5)

markblue=function(d1,
                  d2,
                  vl,
                  ci,
                  ncu2=1,
                  ntrat2=2,
                  nrep2=3,
                  nresp2=5,
                  naf2=6,
                  analysis=TRUE,
                  design="DIC",
                  transf=1,
                  quali=TRUE,
                  grau=1,
                  test="parametric",
                  mcomp="tukey",
                  ylab=expression(mu~cm^2),
                  save.xlsx=FALSE){
  if(is.data.frame(d1)==FALSE){d1=read_excel(d1)}
  if(is.data.frame(d2)==FALSE){d2=read_excel(d2)}
  curva=factor(as.vector(unlist(d1[,1])),unique(unlist(d1[,1])))
  ncurvas=nlevels(curva)
  modelos=c()
  response=as.vector(unlist(d2[,nresp2]))
  curvas=as.vector(unlist(d2[,ncu2]))
  af=as.vector(unlist(d2[,naf2]))
  curvas=factor(curvas,unique(curvas))
  d2a=data.frame(curvas,response)
  xmax=tapply(response,curvas, max)
  xmin=tapply(response,curvas, min)
  for(i in 1:ncurvas){
    maximo=xmax[levels(curvas)[i]]
    minimo=xmin[levels(curvas)[i]]
    abs=as.vector(unlist(d1[,3]))
    ppm=as.vector(unlist(d1[,4]))
    data=data.frame(curva,abs,ppm)
    abst=abs[abs>minimo & abs<maximo]
    ppmt=ppm[abs>minimo & abs<maximo]
    curvat=curva[abs>minimo & abs<maximo]
    abst1=abs[abs>maximo]
    curvat1=curva[abs>maximo]
    ocultos=length(na.omit(abst1[curvat1==levels(curva)[i]]))
    datat=data.frame(curvat,abst,ppmt)
    aa=na.omit(datat[curvat==levels(curva)[i],])
    numeros=as.numeric(rownames(aa))
    minimos=numeros[1]+ocultos-1
    maximos=numeros[length(numeros)]+ocultos+1
    data=data[c(minimos:maximos),]
    modelos[[i]]=with(data,#[curva==levels(curva)[i],],
                      lm(abs~ppm))
  }
  names(modelos)=levels(curva)
  respostas=c()
  models=c()
  for(i in 1:ncurvas){
    resps=d2a[curvas==levels(curva)[i],]
    response=as.vector(unlist(resps[,2]))
    mod=modelos[levels(curva)[i]][[1]]
    b0=coef(mod)[1]
    b1=coef(mod)[2]
    preditos=function(y){x=(y-b0)/b1}
    respostas[[i]]=preditos(response)
    models[[i]]=summary(mod)
  }
  names(models)=levels(curva)
  mgL=unlist(respostas)
  ml=(mgL*vl)/ci
  microl=ml*1000
  microlcm2=microl/af
  if(analysis==FALSE){
    respostas=data.frame(Abs=as.vector(unlist(d2[,nresp2])),
                         mgL,ml,
                         microl,microlcm2)}
  if(analysis==TRUE){
    respostas=data.frame(trat=as.vector(unlist(d2[,ntrat2])),
                         rep=as.vector(unlist(d2[,nrep2])),
                         Abs=as.vector(unlist(d2[,nresp2])),
                         mgL,ml,
                         microl,microlcm2)
    if(design=="DIC"){
      with(respostas,dic.analysis(trat,
                                  microlcm2,
                                  transf = transf,
                                  quali=quali,
                                  grau = grau,
                                  test=test,
                                  mcomp=mcomp,
                                  ylab=ylab))}
    if(design=="DBC"){
      with(respostas,dbc.analysis(trat,
                                  rep,
                                  microlcm2,
                                  transf = transf,
                                  quali=quali,
                                  grau = grau,
                                  test=test,
                                  mcomp = mcomp,
                                  ylab=ylab))}}
  cat("\n")
  if(save.xlsx=="TRUE"){xlsx::write.xlsx(respostas,"result.xlsx")}
  list(models=models,results=respostas)
}
