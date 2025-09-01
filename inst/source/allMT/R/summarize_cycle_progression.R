#' @title Plot a cycle-specific summary graph of maintenance therapy (MT) data for single patient
#'
#' @description Create summary graph with maintenance therapy data.
#'              Weighted mean absolute neutrophil count (ANC) and dose information is calculated and plotted for each cycle.
#'
#' @param input_file_path Path to input csv file for the patient (in quotes)
#' @param anc_range ANC target range as per the protocol: (c(lower threshold, upper threshold)). NOTE: Ensure that units are the same as unit of ANC in the input data.
#' @param unit Choose either "million" or "billion".
#' \itemize{
#'            \item{"million" = million cells/L (x\eqn{10^{6}} cells/L or cells/\eqn{\mu}l)}
#'            \item{"billion" = billion cells/L (x\eqn{10^{9}} cells/L or x\eqn{10^{3}} cells/\eqn{\mu}l)}
#' }
#' @note
#'\itemize{
#'          \item{If there is only one threshold for anc_range parameter, please specify the respective value and keep the other threshold as NA. eg : c(2000, NA)}
#'          \item{Horizontal dotted lines on the graph indicate anc_range thresholds.Red dot represents summarized overall MT data.}
#'       }
#' @return Plot image
#'
#' @seealso [plot_progression()]
#'
#' @examples
#' pat_data = system.file("extdata/processed_data/", "UPN_916.csv", package = "allMT")
#' summarize_cycle_progression(input_file_path = pat_data, anc_range = c(0.75, 1.5),
#'                  unit = "billion")
#' \donttest{
#' summarize_cycle_progression(input_file_path = pat_data,
#'                  anc_range = c(0.8, 2), unit = "billion")
#'
#' # As per BFM protocol (Reference PMID - 15902295):
#' summarize_cycle_progression(input_file_path = pat_data,
#'                  anc_range = c(2, NA),
#'                  unit = "billion")
#'
#' # As per St Jude protocol (Reference PMID - 15902295):
#' summarize_cycle_progression(input_file_path = pat_data,
#'                  anc_range = c(0.8, 2),
#'                  unit = "billion")
#'
#' }
#'
#' @importFrom ggplot2 element_text guides guide_legend arrow unit
#'
#' @export
#'
summarize_cycle_progression <- function(input_file_path, anc_range, unit){

  tryCatch (

    expr = {

      ANC <- Cycle <- MP_adj <- MTX_adj <- N_6MP <- N_MTX <- N_antiMtb <- arrow <- csSM <- wm_ANC <- wm_AntiMtb <- wt <- NULL

      file_name <- NULL

      if(missing(input_file_path)){
        stop("Please provide a file path for the 'input_file_path' argument")
      }

      if(utils::file_test("-f", input_file_path)){

        message("NOTE: Analyzing provided input file")

      }else{
        stop("Please provide a valid file path for the 'input_file_path' argument")
      }

      if(missing(anc_range)){
        stop("Please provide a ANC range for the 'anc_range' argument")
      }

      if(missing(unit)){
        stop("Please provide a unit for the 'unit' argument")
      }


      mt_csv_path <- input_file_path

      # Initializing cycle list
      cycle_list <- c("1", "2", "3", "4", "5", "6", "7", "8")

      # Initializing function to plot graph
      Graph <- function(MT_cyc, n, ANC_LowerBoundary, ANC_UpperBoundary, MR_mean){

        csSM_plot <- ggplot2::ggplot(MT_cyc ,aes(x=wm_AntiMtb, y=wm_ANC, color = Cycle, size = Cycle, group = 1))+
          ggplot2::labs(x= "Weighted mean Antimetabolite dose intensity (%)",
                        # y= expression(paste("Weighted mean ANC (", x10^{n}, "cells/L)")))+
                        y= bquote("Weighted mean ANC (x10"^~.(n)~ "cells/L)"))+
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,max(MT_cyc$wm_AntiMtb, na.rm = T)+ 20),
                                      breaks = scales::breaks_width(20))+
          ggplot2::scale_y_continuous(limits = c(0,(max(MT_cyc$wm_ANC, na.rm = TRUE)+500)),expand = c(0,0),
                                      breaks = scales::breaks_width(500))+
          ggplot2::annotate("rect", xmin = 80, xmax = 120, ymin = ANC_LowerBoundary, ymax = ANC_UpperBoundary, fill = "grey",
                            alpha = 0.25)+
          ggplot2::theme_classic()+
          ggplot2::theme(legend.position = 'none')+
          ggplot2::guides(color = guide_legend(ncol = 2))+
          ggplot2::scale_linetype(name = "optimal ANC")+
          ggplot2::geom_hline(yintercept = ANC_LowerBoundary, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_hline(yintercept = ANC_UpperBoundary, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_vline(xintercept = 80, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_vline(xintercept = 120, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_point()+
          ggplot2::geom_path(size = 0.5, arrow = arrow(length = unit(0.15, "inches")))+
          ggplot2::scale_color_manual(labels = NULL,
                                      values = c("#d9d9d9","#D3D3D3", "#A9A9A9", "#888888", "#707070", "#505050",
                                                 "#303030", "#000000", "Red"))+
          ggplot2::geom_point(data = MR_mean, aes(x = wm_AntiMtb, y = wm_ANC), size = 3)+
          ggplot2::theme(axis.title.x = element_text(size = 11, color = "Black"),
                         axis.title.y = element_text(size = 11, color = "Black"),
                         axis.text.x = element_text(size = 11, color = "Black"),
                         axis.text.y = element_text(size = 11, color = "Black"))+
          ggplot2::theme(plot.margin = unit(c(0.6,0.75,0.4,0.3), "cm"))

        return(csSM_plot)
      }

      # Reading patient csv file
      MR_0 <- utils::read.csv(mt_csv_path)

      # Calculating weights
      W <- NULL
      for (y in 1:(nrow(MR_0)-1))
      {
        Wt <- (MR_0$Weeks[[y+1]] - MR_0$Weeks[[y]])
        W <- rbind(W, Wt)
      }

      W <- rbind(W, 1)

      # Calculating and adding weighted mean data to data frame
      MR <- MR_0%>%
        dplyr::mutate(wt = W)%>%
        dplyr::mutate(wt_ANC = ANC*wt)%>%
        dplyr::mutate(wt_ANC = ANC*wt)%>%
        dplyr::mutate(N_6MP = MP_adj/100)%>%
        dplyr::mutate(N_MTX = MTX_adj/100)%>%
        dplyr::mutate(N_antiMtb = N_6MP*N_MTX)%>%
        dplyr::mutate(wt_antiMtb = N_antiMtb*wt)
      # filter(wt<=8)


      # Calculating summary measures per cycle for patient
      MT_cyc <- data.frame()[1:1, ]
      for (b in seq(cycle_list)) {

        MR_cyc <- dplyr::filter(MR, MR$Cycle == b)

        cyc <- data.frame(Cycle = cycle_list[b],
                          wm_ANC = (sum(MR_cyc$wt_ANC)/sum(MR_cyc$wt)),
                          wm_AntiMtb = ((sum(MR_cyc$wt_antiMtb)/sum(MR_cyc$wt))*100))

        MT_cyc <- rbind(MT_cyc, cyc)
      }

      # Calculating overall summary measures for patient
      MR_mean <- data.frame(Cycle = "Overall",
                            wm_ANC = (sum(MR$wt_ANC)/sum(MR$wt)),
                            wm_AntiMtb = ((sum(MR$wt_antiMtb)/sum(MR$wt))*100))

      MT_cyc$wm_ANC <- as.numeric(MT_cyc$wm_ANC)

      # Creating graph and adjusting scales/labels as per unit provided
      if(unit == "million"){
        if(anc_range[1] < 1 |  anc_range[2] < 1){
          message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
        }

        csSM_plot <- Graph(MT_cyc = MT_cyc, n = 6, ANC_LowerBoundary =  anc_range[1],
                           ANC_UpperBoundary = anc_range[2], MR_mean = MR_mean)
        csSM_plot <- csSM_plot+
          ggplot2::scale_y_continuous(limits = c(0,(max(MT_cyc$wm_ANC, na.rm = TRUE)+500)),expand = c(0,0),
                                      breaks = scales::breaks_width(500))


      }
      if(unit == "billion"){
        if(anc_range[1] > 100 |  anc_range[2] > 100){
          message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
        }
        csSM_plot <-  Graph(MT_cyc = MT_cyc, n = 9, ANC_LowerBoundary =  anc_range[1],
                            ANC_UpperBoundary = anc_range[2], MR_mean = MR_mean)

        csSM_plot <- csSM_plot+
          ggplot2::scale_y_continuous(limits = c(0,(max(MT_cyc$wm_ANC, na.rm = TRUE)+0.5)),expand = c(0,0),
                                      breaks = scales::breaks_width(0.5))



      }

      message("Cycle progression summary graph created")
      # Returning plot
      return(csSM_plot)

    },
    error = function(e) {
      message("Error")
      print(e)
    },
    warning = function(w) {
      message("Warning")
      print(w)
    },
    finally = {
      message("Quitting")
      message("Bye Bye: Did you know that Saturn is second largest planet in our solar system :)?")
    }
  )

}
