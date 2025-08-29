plot_selection = function(object){
  dimSelec = object$selec.var ; nlam = length(dimSelec) ; dim.aic = object$aic.var  
  
  occurrence.table = data.frame(table(unlist(dimSelec))) ; colnames(occurrence.table) = c("Covariable", "Percentage")
  occurrence.table$Percentage =occurrence.table$Percentage*100/nlam

   if (length(dim.aic)!=0){
     occurrence.table=merge(x=occurrence.table, y=data.frame(Covariable = as.factor(dim.aic), Metric = 'AIC'), by="Covariable", all.x = TRUE)
   }else{ occurrence.table$Metric = rep(NA, nrow(occurrence.table))}
  
  occurrence.table = occurrence.table[order(-occurrence.table$Percentage),,drop=FALSE]
  occurrence.table$Covariable = factor(occurrence.table$Covariable,
                                       levels = unique(occurrence.table$Covariable))
  plt_occ = ggplot2::ggplot(data = occurrence.table, aes(x = occurrence.table$Covariable, y = occurrence.table$Percentage, color = occurrence.table$Metric)) +
    ggplot2::geom_bar(stat = 'identity', fill = "lightpink", size=2) +
    ggplot2::scale_colour_manual(na.translate = F, values = 'indianred4') +
    ggplot2::ylab('Percentage of selection') +
    ggplot2::theme_bw() + 
    ggplot2::theme( panel.grid.major = element_line(size = 0.8, linetype = 'solid',
                                           colour = "darkgrey"), 
           panel.grid.minor = element_line(size = 0.2, linetype = 'solid',
                                           colour = "darkgrey"),
          axis.text.x = element_text(size = 22, face = 'bold'), 
          axis.text.y = element_text(size = 20), 
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 20),
          legend.text =  element_text(size = 21),
          legend.title = element_blank(),
          legend.position = 'bottom'
         )
  return(plt_occ)
}   