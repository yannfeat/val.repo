#' Graph: Point graph for one factor model 2
#'
#' @description This is a function of the point graph for one factor
#' @author Gabriel Danilo Shimizu, \email{gabrield.shimizu@gmail.com}
#' @author Leandro Simoes Azeredo Goncalves
#' @author Rodrigo Yudi Palhaci Marubayashi
#' @param model DIC, DBC or DQL object
#' @param theme ggplot2 theme
#' @param horiz Horizontal Column (\emph{default} is TRUE)
#' @param pointsize Point size
#' @param pointshape Format point (default is 16)
#' @param vjust vertical adjusted
#' @export
#' @return Returns a point chart for one factor
#' @seealso \link{barplot_positive}, \link{plot_TH}, \link{corgraph}, \link{spider_graph}, \link{line_plot}
#' @examples
#' data("laranja")
#' a=with(laranja, DBC(trat, bloco, resp,
#'        mcomp = "sk",angle=45,
#'        ylab = "Number of fruits/plants"))
#' seg_graph2(a,horiz = FALSE)

seg_graph2=function(model,
                    theme=theme_gray(),
                    pointsize=4,
                    pointshape=16,
                    horiz=TRUE,
                    vjust=-0.6){
  requireNamespace("ggplot2")
  data=model$plot$data
  media=data$media
  desvio=data$desvio
  trats=data$trats
  limite=data$limite
  letra=data$letra
  groups=data$groups
  sup=model$sup
  textsize=model$textsize
  if(horiz==TRUE){
  graph=ggplot(data,aes(y=trats,
                          x=media))+theme+
      geom_errorbar(aes(xmin=media-desvio,
                        xmax=media+desvio),width=0,linewidth=0.8)+
      geom_point(size=pointsize,shape=16, fill="black", color="black")+
      geom_text(aes(x=media,
                    y=trats,
                    label = letra),vjust=vjust,family=model$family)+
      labs(y=parse(text=model$xlab),
           x=parse(text=model$ylab))+
      theme(axis.text = element_text(size=12,color="black"),
            strip.text = element_text(size=12),
            legend.position = "none")+
      scale_y_discrete(limits=trats)+
      xlim(layer_scales(model$plot)$y$range$range)}
  if(horiz==FALSE){
    graph=ggplot(data,aes(x=trats,
                          y=media))+theme+
      geom_errorbar(aes(ymin=media-desvio,
                        ymax=media+desvio),width=0,linewidth=0.8)+
      geom_point(size=pointsize,shape=16, fill="black", color="black")+
      geom_text(aes(y=media,
                     x=trats,
                     label = letra),vjust=vjust,angle=90,family=model$family)+
      labs(x=parse(text=model$xlab),
           y=parse(text=model$ylab))+
      theme(axis.text = element_text(size=textsize,color="black"),
            strip.text = element_text(size=textsize),
            legend.position = "none")+
      scale_x_discrete(limits=trats)+
      ylim(layer_scales(model$plot)$y$range$range)}
  graph
}

