#' Graph: Point graph for one factor
#'
#' @description This is a function of the point graph for one factor
#' @author Gabriel Danilo Shimizu, \email{gabrield.shimizu@gmail.com}
#' @author Leandro Simoes Azeredo Goncalves
#' @author Rodrigo Yudi Palhaci Marubayashi
#' @param model DIC, DBC or DQL object
#' @param fill fill bars
#' @param horiz Horizontal Column (\emph{default} is TRUE)
#' @param pointsize Point size
#' @export
#' @return Returns a point chart for one factor
#' @seealso \link{barplot_positive}, \link{plot_TH}, \link{corgraph}, \link{spider_graph}, \link{line_plot}
#' @examples
#' data("laranja")
#' a=with(laranja, DBC(trat, bloco, resp,
#'        mcomp = "sk",angle=45,sup=10,
#'        ylab = "Number of fruits/plants"))
#' seg_graph(a,horiz = FALSE)


seg_graph=function(model,
                   fill="lightblue",
                   horiz=TRUE,
                   pointsize=4.5){
  requireNamespace("ggplot2")
  data=model$plot$data
  media=data$media
  desvio=data$desvio
  trats=data$trats
  limite=data$limite
  letra=data$letra
  groups=data$groups
  sup=model$sup

  if(horiz==TRUE){
    graph=ggplot(data,aes(y=trats,
                          x=media))+
      model$plot$theme+
      geom_errorbar(aes(xmin=media-desvio,
                        xmax=media+desvio),width=0.2,linewidth=0.8)+
      geom_point(size=pointsize,shape=21, fill=fill, color="black")+
      geom_text(aes(x=media+desvio+sup,
                    y=trats,
                    label = letra),hjust=0,family=model$family)+
      labs(y=parse(text=model$xlab),
           x=parse(text=model$ylab))+
      theme(axis.text = element_text(size=model$textsize,color="black"),
            strip.text = element_text(size=model$textsize),
            legend.position = "none")+
      scale_y_discrete(limits=trats)+
      xlim(layer_scales(model$plot)$y$range$range)}
  if(horiz==FALSE){
    graph=ggplot(data,aes(x=trats,
                          y=media))+
      model$plot$theme+
      geom_errorbar(aes(ymin=media-desvio,
                        ymax=media+desvio),width=0.2,linewidth=0.8)+
      geom_point(fill=fill,size=pointsize,shape=21,color="black")+
      geom_text(aes(y=media+desvio+sup,
                    x=trats,
                    label = letra),vjust=0,family=model$family)+
      labs(x=parse(text=model$xlab),
           y=parse(text=model$ylab))+
      theme(axis.text = element_text(size=model$textsize,color="black"),
            strip.text = element_text(size=model$textsize),
            legend.position = "none")+
      scale_x_discrete(limits=trats)+
      ylim(layer_scales(model$plot)$y$range$range)}
  graph
}
