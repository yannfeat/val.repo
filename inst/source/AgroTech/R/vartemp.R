#' Temporal variability graph of weather conditions
#'
#' @description This is a function to check weather conditions in agricultural spraying
#'
#' @param file Excel file (xlsx)
#' @param nx Time
#' @param ny Weather conditions
#' @param variable Variable name
#' @param ylab y axis (Dependent)
#' @param xlab x axis (Independent)
#' @param size.text Size text (\emph{default} is 12)
#' @param size.title Size title (\emph{default} is 12)
#' @param size.strip Size strip (\emph{default} is 12)
#' @param size.lty Size line (\emph{default} is 0.7)
#'
#' @return Returns graph of ggplot2
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
#' @examples
#' data("example_meteorological")
#' vartemp(example_meteorological)

vartemp=function(file,
                nx=1,
                ny=2,
                variable=NA,
                ylab="Dependent",
                xlab="Independent",
                size.text=12,
                size.title=12,
                size.strip=12,
                size.lty=0.7){
  requireNamespace("readxl")
  requireNamespace("ggplot2")
  if(is.data.frame(file)==TRUE){dados=file}else{dados=read_excel(file)}
  time=unlist(dados[,nx])
  resp=unlist(dados[,ny])
  data1=data.frame(time=time,
                   resp=resp)
  a=ggplot(data1, aes(x=time))+
    geom_line(aes(y=resp),size=size.lty)+ylab(ylab)+xlab(xlab)+
    facet_wrap(~as.character(variable))+theme_bw()+
    theme(axis.text = element_text(size=size.text,color="black"),
          strip.text = element_text(size=size.strip,color="black"),
          axis.title = element_text(size=size.title,color="black"))
  list(a)[[1]]
}
