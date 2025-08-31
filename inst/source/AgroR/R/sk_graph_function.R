#' Graph: Scott-Knott graphics
#'
#' @description This is a function of the bar graph for the Scott-Knott test
#' @author Gabriel Danilo Shimizu, \email{gabrield.shimizu@gmail.com}
#' @author Leandro Simoes Azeredo Goncalves
#' @author Rodrigo Yudi Palhaci Marubayashi
#' @param model DIC, DBC or DQL object
#' @param horiz Horizontal Column (\emph{default} is TRUE)
#' @param fill.label fill Label box fill color
#' @export
#' @return Returns a bar chart with columns separated by color according to the Scott-Knott test
#' @seealso \link{barplot_positive}, \link{plot_TH}, \link{corgraph}, \link{spider_graph}, \link{line_plot}
#' @examples
#' data("laranja")
#' a=with(laranja, DBC(trat, bloco, resp,
#'        mcomp = "sk",angle=45,
#'        ylab = "Number of fruits/plants"))
#' sk_graph(a,horiz = FALSE)
#' library(ggplot2)
#' sk_graph(a,horiz = TRUE)+scale_fill_grey(start=1,end=0.5)

sk_graph=function(model,
                  horiz=TRUE,
                  fill.label="lightyellow"){
  requireNamespace("ggplot2")
  data=model$plot$data
  family=model$family
  media=data$media
  desvio=data$desvio
  trats=data$trats
  limite=data$limite
  letra=data$letra
  groups=data$groups
  sup=model$sup
  ploterror=model$errorbar
  labelsize=model$labelsize
  textsize=model$textsize
  # if(transf==FALSE){data=data[,c(5,1,2)]}
  # if(transf==TRUE){data=data[,c(6,3,2)]}
  if(horiz==TRUE){
    graph=ggplot(data,aes(y=as.vector(trats),
                          x=as.vector(media)))+
      model$plot$theme+
      geom_col(aes(fill=as.vector(groups)),
               width=model$width.column,
               color="black")
    if(ploterror==TRUE){graph=graph+geom_errorbar(aes(xmax=media+desvio,xmin=media-desvio),
                                                  width=model$plot$layers[3][[1]]$geom_params$width)+
      geom_label(aes(x=as.vector(media)+sup+desvio,
                     y=as.vector(trats),
                     label = letra),family=family,size=labelsize,
                 fill=fill.label,hjust=0)}
    if(ploterror==FALSE){graph=graph+geom_label(aes(x=as.vector(media)+sup,
                                                    y=as.vector(trats),
                                                    label = letra),family=family,
                                                fill=fill.label,hjust=0)}
    graph=graph+
      labs(y=parse(text=model$plot$labels$x),
           x=parse(text=model$plot$labels$y))+
      theme(axis.text = element_text(size=textsize,color="black"),
            strip.text = element_text(size=textsize),
            legend.position = "none")+
      scale_y_discrete(limits=data$trats)+
      xlim(layer_scales(model$plot)$y$range$range*1.1)}
  if(horiz==FALSE){
    graph=ggplot(data,aes(x=as.vector(trats),
                          y=as.vector(media)))+
      model$plot$theme+
      geom_col(aes(fill=as.vector(groups)),
               width=model$width.column,
               color="black")
    if(ploterror==TRUE){graph=graph+geom_errorbar(aes(ymax=media+desvio,ymin=media-desvio),
                                                  width=model$plot$layers[3][[1]]$geom_params$width)+
      geom_label(aes(y=as.vector(media)+sup+desvio,
                     x=as.vector(trats),
                     label = letra),family=family,size=labelsize,
                 fill=fill.label)}
    if(ploterror==FALSE){graph=graph+geom_label(aes(y=as.vector(media)+sup,
                                                    x=as.vector(trats),
                                                    label = letra),family=family,
                                                fill=fill.label,hjust=0)}
    graph=graph+
      labs(x=parse(text=model$plot$labels$x),
           y=parse(text=model$plot$labels$y))+
      theme(axis.text = element_text(size=textsize,color="black"),
            strip.text = element_text(size=textsize),
            legend.position = "none")+
      scale_x_discrete(limits=data$trats)+
      ylim(layer_scales(model$plot)$y$range$range)}
  graph
}
