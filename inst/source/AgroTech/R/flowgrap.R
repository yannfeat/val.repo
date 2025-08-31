#' Flow graphic of nozzles on spray bar
#'
#' @description This is a function to check the conditions of the spray nozzles
#'
#' @param file Numerical vector with the flows
#' @param pointsize Point size (\emph{default} 3.5)
#' @param xsup Upper limit
#' @param xinf Bottom limit
#' @param pointcolor Point color (red)
#' @param xlab x axis legend
#' @param ylab y axis legend
#'
#' @return Returns graph of ggplot2
#'
#' @import ggplot2
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom stats na.omit
#' @importFrom stats coef
#' @importFrom readxl read_excel
#' @importFrom ggrepel geom_text_repel
#'
#' @author Rodrigo Yudi Palhaci Marubayashi, \email{marubayashi@uel.br}
#' @author Gabriel Danilo Shimizu
#' @author Otavio Jorge Grigoli Abi Saab
#'
#' @references
#' ANDEF Associacao Nacional de Defesa Vegetal. Manual de tecnologia de aplicacao de produtos fitossanitarios. Campinas: Linea Creativa, 2004. 50p.
#'
#' BOLLER, W.; RAETANO, C. G. Bicos e pontas de pulverizacao de energia hidraulica, regulagens e calibracao de pulverizadores de barras. In: ANTUNIASSI, U. R.; BOLLER, W. (Organizadores). Tecnologia de aplicacao para culturas anuais. Passo Fundo: Aldeia Norte; Botucatu: FEPAF, 2011. p.51-82.
#'
#' SPRAYING SYSTEMS CO. Catalogo 51A-PT - Produtos de pulverizacao para agricultura. Wheaton: Spraying Systems Co., 2014. 160p.
#'
#' @export
#'
#' @examples
#' resp=c(881,854,865,876,906.3,
#' 874.7,868.3,878.7,872.7,901.7,
#' 823.3,889.7,861.3,900.3,890.3,
#' 886.7,916.7,872,912.7,894)
#' flowgrap(resp)
#' # flowgrap("file.xlsx")


flowgrap=function(file,
                  pointsize=3.5,
                  xsup=1.1,
                  xinf=0.9,
                  pointcolor="red",
                  xlab="Nozzle number",
                  ylab=NA){
  requireNamespace("crayon")
  if(is.na(ylab)==TRUE){ylab=expression("Nozzle flow"~(mL~min^{-1}))}
  if(length(file)==1){dados=read_excel(file)
  x=as.vector(unlist(dados[,1]))}else{x=file}
  ponta=1:length(x)
  d=data.frame(ponta,x)
  m1=mean(x)
  m2=mean(x)*xsup
  m3=mean(x)*xinf
  outlier=d[x<m3 | x>m2,]
  requireNamespace("ggplot2")
  requireNamespace("ggrepel")
  graph=ggplot(d,aes(x = ponta, y = x)) +
    geom_hline(yintercept = m1, #linetype = "dashed",
               color="black",size=1) +
    geom_hline(yintercept = m2, linetype = 2,color="black",size=1) +
    geom_hline(yintercept = m3, linetype = 2,color="black",size=1) +
    theme_classic()+
    geom_point(data=outlier,aes(x=ponta,y=x),color=pointcolor,size=pointsize)+
    geom_text_repel(data=outlier,aes(x=ponta,y=x,label=ponta))+
    labs(x = xlab,
         y = ylab) +
    geom_point(shape=21,size=pointsize) +
    scale_x_continuous(breaks = seq(1,length(x),1))
  print(graph)
  if(length(outlier$x)==0){print("No problem nozzles")}else{
    message(black("\n-----------------------------------------------\n"))
    message(black("Problem 1: Worn nozzles "))
    message(outlier$ponta[outlier$x>m2])
    message(black("\n-----------------------------------------------"))
    message("\n")
    message(black("Problem 2: Clogged nozzles "))
    message(outlier$ponta[outlier$x<m3])
    message(black("\n-----------------------------------------------"))}
}
