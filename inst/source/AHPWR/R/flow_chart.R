#'@title creates a hierarchical structure
#'@name flow_chart
#'@author Luciane Ferreira Alcoforado
#

#'@description  Function to build the Diagram of hierarchies
#'
#'@param names is a vector with names for goal, criteria and choices in this exact sequence
#'@param c is a integer number of criteria, c>=2
#'@param a is a integer number of choices, a>=2
#'@return Returns Diagram of hierarchies
#'@examples
#'p=flow_chart(names=NULL, a=2, c=2)
#'p
#'
#'p=flow_chart(names=NULL, a=2, c=3)
#'p+ggplot2::theme_void()
#'
#'flow_chart(names=c("G", "cost", "time", "hour", "home", "beach"),c=3, a=2)
#'
#'@importFrom igraph graph_from_data_frame
#'@importFrom igraph layout_as_tree
#'@importFrom igraph vertex_attr
#'@import ggplot2
#'@import dplyr
#'@importFrom tidyr pivot_longer
#'@importFrom tibble as_tibble

#'@export
#'

flow_chart = function(names, c, a){
  #require(ggplot2)
  #require(igraph)  require(tibble)  require(tidyr)  require(dplyr)
  if (any(c < 2))  stop("need positive value c >=2")
  if (any(a < 2)) stop("need positive value a >=2")
  if(is.null(names)) names=c("Goal", paste0("c",1:c), paste0("a",1:a))
  dt=data.frame(from = c(rep(names[1],c), rep(names[2:(c+1)],rep(a,c))),to=c(names[2:(c+1)], rep(names[(c+2):(c+a+1)],c)))

  g = igraph::graph_from_data_frame(dt, directed = TRUE)
coords = igraph::layout_as_tree(g)
colnames(coords) = c("x", "y")

coords[(c+2): (c+a+1),1]=coords[(c+2): (c+a+1),1]+1
step = igraph::vertex_attr(g, "name")
tp = factor(c("Goal", rep("criteria",c), rep("choices",a)))
output_df = tibble::as_tibble(coords)
x = output_df$x #na prática não precisa, coloquei por erro no cran
y = output_df$y #na prática não precisa, coloquei por erro no cran

output_df = output_df %>%  dplyr::mutate(
         #label = gsub("\\d+$", "", step),
         step = step,
         label = step,
         x = x*1,
         type = tp)
plot_nodes = output_df %>%
  dplyr::mutate(xmin = x - 0.25,
         xmax = x + 0.25,
         ymin = y - 0.25,
         ymax = y + 0.25)
plot_edges = dt %>%
  dplyr::mutate(id = dplyr::row_number()) %>%
  tidyr::pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  dplyr::left_join(plot_nodes, by = "step") %>%
  dplyr::select(-c(label, type, y, xmin, xmax)) %>%
  dplyr::mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  dplyr::select(-c(ymin, ymax))

p = ggplot2::ggplot() + ggplot2::coord_fixed()+
  ggplot2::geom_rect(data = plot_nodes,
            mapping = ggplot2::aes(xmin = xmin, ymin = ymin,
                          xmax = xmax, ymax = ymax,
                          fill = type, colour = type),
            alpha = 0.5)

p = p +
  ggplot2::geom_text(data = plot_nodes,
            mapping = ggplot2::aes(x = x, y = y, label = label),
            #family = "Times New Roman",
            color = "#685c50")

p = p +
  ggplot2::geom_path(data = plot_edges,
            mapping = ggplot2::aes(x = x, y = y, group = id),
            colour = "#685c50",
            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"))+
            ggplot2::labs(title = "Hierarchical Tree of Decision",
       caption = "R-package AHPWR, 2022")+
          ggplot2::theme(axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank())



return(p)}

