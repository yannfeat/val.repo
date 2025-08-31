#' Graph: Barplot for Dunnett test
#' @export
#' @description The function performs the construction of a column chart of Dunnett's test.
#' @param output.dunnett Numerical or complex vector with treatments
#' @param sup Number of units above the standard deviation or average bar on the graph
#' @param ylab Variable response name (this argument uses the \emph{parse} function)
#' @param xlab Treatments name (this argument uses the \emph{parse} function)
#' @param fill Fill column. Use vector with two elements c(control, different treatment, no difference treatment)
#' @param add.mean Plot the average value on the graph (\emph{default} is TRUE)
#' @param round Number of cells
#' @return Returns a column chart of Dunnett's test. The colors indicate difference from the control.
#' @importFrom multcomp glht
#' @importFrom multcomp mcp
#' @examples
#'
#' #====================================================
#' # randomized block design in factorial double
#' #====================================================
#' library(AgroR)
#' data(cloro)
#' attach(cloro)
#' respAd=c(268, 322, 275, 350, 320)
#' a=FAT2DBC.ad(f1, f2, bloco, resp, respAd,
#'              ylab="Number of nodules",mcomp="sk")
#' data=rbind(data.frame(trat=paste(f1,f2,sep = ""),bloco=bloco,resp=resp),
#'            data.frame(trat=c("Test","Test","Test","Test","Test"),
#'                       bloco=unique(bloco),resp=respAd))
#' a= with(data,dunnett(trat = trat,
#'                   resp = resp,
#'                   control = "Test",
#'                   block=bloco,model = "DBC"))
#'  bar_dunnett(a)


bar_dunnett=function(output.dunnett,
                     ylab="Response",
                     xlab="",
                     fill=c("white","#F8766D","#00BFC4"),
                     sup=NA,
                     add.mean=TRUE,
                     round=2){
  resp=output.dunnett$resp
  trat=output.dunnett$trat
  xlab=parse(text=gsub(" ","~",xlab))
  ylab=parse(text=gsub(" ","~",ylab))
  if(is.na(sup[1])==TRUE){sup=0.1*mean(resp)}
  controle=output.dunnett$control
  medias=tapply(resp,trat,mean)
  medias=medias[order(medias,decreasing = TRUE)]
  ordem=as.vector(names(medias))
  ordem1=c(controle,ordem[!ordem==controle])
  trat=factor(trat,ordem1)
  medias=tapply(resp,trat,mean)
  trat1=factor(names(medias),levels = levels(trat))
  requireNamespace("ggplot2")
  estimativa=output.dunnett$plot$data[order(output.dunnett$plot$data$Estimate,decreasing = TRUE),]
  if(add.mean==FALSE){ggplot(data.frame(trat1,medias))+output.dunnett$plot$theme+
      geom_col(aes(x=trat1,y=medias,
                   fill=c(" ",ifelse(estimativa$sig=="*","*","ns"))),
               color="black",show.legend = FALSE)+
      labs(x=parse(text=xlab),y=parse(text=ylab))+
      geom_label(aes(x=trat1,y=medias+sup,
                     label=c(" ",ifelse(estimativa$sig=="*","*","ns"))),
                 family=output.dunnett$family)+
      scale_fill_manual(values=fill)}
  if(add.mean==TRUE){ggplot(data.frame(trat1,medias))+output.dunnett$plot$theme+
      geom_col(aes(x=trat1,y=medias,
                   fill=c(" ",ifelse(estimativa$sig=="*","*","ns"))),
               color="black",show.legend = FALSE)+
      labs(x=parse(text=xlab),y=parse(text=ylab))+
      geom_label(aes(x=trat1,y=medias+sup,
                     label=paste(round(medias,round),
                                 c(" ",ifelse(estimativa$sig=="*","*","ns")))),
                 family=output.dunnett$family)+
      scale_fill_manual(values=fill)}
}


