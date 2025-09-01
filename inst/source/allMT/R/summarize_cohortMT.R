#' @title Plot summarized maintenance therapy (MT) data for a cohort
#'
#' @description Create an integrated summary graph illustrating
#'              weighted mean absolute neutrophil count (ANC) and dose information for each patient in the cohort.
#'
#' @param input_files_path Path to folder with MT csv files (in quotes)
#' @param anc_range ANC target range as per the protocol: (c(lower threshold, upper threshold)). NOTE: Ensure that units are the same as unit of ANC in the input data.
#' @param unit Choose either "million" or "billion".
#' \itemize{
#'            \item{"million" = million cells/L (x\eqn{10^{6}} cells/L or cells/\eqn{\mu}l)}
#'            \item{"billion" = billion cells/L (x\eqn{10^{9}} cells/L or x\eqn{10^{3}} cells/\eqn{\mu}l)}
#' }
#' @param dose_intensity_threshold  numeric value of reference drug dose intensity (%). (optional)
#'
#' @note
#'  \itemize{
#'            \item{If there is only one threshold for anc_range or dose_intensity_range parameters, please specify the respective value and keep the other threshold as NA. eg : c(2000, NA), c(100, NA)}
#'            \item{Horizontal dotted lines on the graph indicate anc_range thresholds.Red dot represents summarized overall MT data.}
#'         }
#' @return Summary graph
#'
#' @seealso [compare_cohorts()]
#'
#' @examples
#' cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#' summarize_cohortMT (input_files_path = cohort_path, unit = "billion",
#'                     anc_range = c(0.75, 1.5), dose_intensity_threshold = c(80, 100))
#'
#' \donttest{
#' summarize_cohortMT(input_files_path = cohort_path,
#'                  unit = "billion", anc_range = c(0.8, 2), dose_intensity_threshold = 150)
#'
#' # As per BFM protocol (Reference PMID - 15902295):
#' summarize_cohortMT (input_files_path = cohort_path,
#'                  unit = "billion", anc_range = c(2, NA), dose_intensity_threshold = 150)
#'
#' # As per St Jude protocol (Reference PMID - 15902295):
#' summarize_cohortMT (input_files_path = cohort_path,
#'                  unit = "billion", anc_range = c(0.8, 2), dose_intensity_threshold = 100)
#'
#'}
#'
#' @export
summarize_cohortMT <- function(input_files_path, anc_range,
                               unit, dose_intensity_threshold){
  tryCatch(
    expr = {

      if(missing(input_files_path)){
        stop("Please provide a folder path for the 'input_files_path' argument")
      }

       if(!utils::file_test("-d", input_files_path)){
        stop("Please provide a folder path for the 'input_files_path' argument")
      }

      if(utils::file_test("-d", input_files_path)){
        message("NOTE: Including all files in input folder")
        pat_list <- list.files(input_files_path)
      }

      if(missing(anc_range)){
        stop("Please provide a ANC range for the 'anc_range' argument")
      }

      if(missing(unit)){
        stop("Please provide a unit for the 'unit' argument")
      }

      if(missing(dose_intensity_threshold)){
        stop("Please provide a value for the 'dose_intensity_threshold' argument")
      }

      ANC<- Group<- N_6MP<- N_MTX<- N_antiMtb <- if_else <- wm_ANC <- wm_AntiMtb <- wt <- NULL

      # Initializing function to plot graph
      Graph <- function(MR_d, n, ANC_LowerBoundary, ANC_UpperBoundary, DI_threshold){
        SM_plot <- ggplot2::ggplot(MR_d,aes(x= wm_AntiMtb*100, y= wm_ANC))+
          ggplot2::labs(x= "Weighted mean Antimetabolite dose intensity (%)",
                        y= bquote("Weighted mean ANC (x10"^~.(n)~ "cells/L)"))+
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,250))+
          # ggplot2::scale_y_continuous(limits = c(0,4000),expand = c(0,0), breaks = scales::breaks_width(500))+
          ggplot2::annotate("rect", xmin = DI_threshold, xmax = 250, ymin = ANC_LowerBoundary,
                            ymax = ANC_UpperBoundary, fill = "grey",alpha = 0.5)+
          ggplot2::theme_classic()+
          ggplot2::scale_linetype(name = "optimal ANC")+
          ggplot2::geom_hline(yintercept = ANC_LowerBoundary, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_hline(yintercept = ANC_UpperBoundary, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_vline(xintercept = DI_threshold, linetype = 2, size = 0.4, color = "black")+
          ggplot2::geom_point(shape = 16, colour = "Black")+
          ggplot2::theme(legend.background = element_rect(colour = 'dark grey',
                                                          fill = 'white', linetype='solid', size = 0.2))+
          ggplot2::theme(plot.margin = unit(c(0.6,0.75,0.4,0.3), "cm"))

        return(SM_plot)
      }

      MR_d <- NULL

      # For each patient:
      for (a in seq(pat_list)) {

        # Reading patient MT csv sheet
        MR <- utils::read.csv(paste0(input_files_path, pat_list[a]))

        # Calculating weights
        W <- NULL
        for (i in 1:(nrow(MR)-1)) {
          Wt <- (MR$Weeks[[i+1]] - MR$Weeks[[i]])
          W <- rbind(W, Wt)
        }

        W <- rbind(W, 1)

        # Calculating and adding weighted mean data to data frame
        MR_b <- MR%>%
          dplyr::mutate(wt = W)%>%
          dplyr::mutate(wt_ANC = ANC*wt)%>%
          dplyr::mutate(N_6MP = MR$MP_adj/100)%>%
          dplyr::mutate(N_MTX = MR$MTX_adj/100)%>%
          dplyr::mutate(N_antiMtb = N_6MP*N_MTX) %>%
          dplyr::mutate(wt_antiMtb = N_antiMtb*wt)%>%
          dplyr::mutate(wt_MP = N_6MP*wt) %>%
          dplyr::mutate(wt_MTX = N_MTX*wt)
        # filter(.,wt<=8)

        # Calculating overall summary measures for patient
        MR_c <- data.frame(Pat_ID = gsub(".csv", "", gsub("_", "/", pat_list[a])),
                           wm_ANC = (sum(MR_b$wt_ANC)/sum(MR_b$wt)),
                           wm_AntiMtb = (sum(MR_b$wt_antiMtb)/sum(MR_b$wt)),
                           wm_MP = (sum(MR_b$wt_MP)/sum(MR_b$wt)),
                           wm_MTX = (sum(MR_b$wt_MTX)/sum(MR_b$wt)))

        # Combining data for all patients
        MR_d <- rbind(MR_d, MR_c)
      }

      # Creating graph and adjusting scales/labels as per unit provided
      if(unit == "million") {
        if(anc_range[1] < 1 |  anc_range[2] < 1){
          message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
        }
        SM_plot <- Graph(MR_d, n = 6,
                         ANC_LowerBoundary =  anc_range[1], ANC_UpperBoundary = anc_range[2],
                         DI_threshold = dose_intensity_threshold)
        SM_plot <- SM_plot+
          ggplot2::scale_y_continuous(limits = c(0,4000),expand = c(0,0), breaks = scales::breaks_width(500))


      }
      if(unit == "billion") {
        if(anc_range[1] > 100 |  anc_range[2] > 100){
          message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
        }

        SM_plot <- Graph(MR_d, n = 9,
                         ANC_LowerBoundary =  anc_range[1], ANC_UpperBoundary = anc_range[2],
                         DI_threshold = dose_intensity_threshold)
        SM_plot <- SM_plot+
          ggplot2::scale_y_continuous(limits = c(0,4),expand = c(0,0), breaks = scales::breaks_width(0.5))

      }

      # Returning plot
      message("Cohort summarized MT data graph created")
      return(SM_plot)

    },
    error = function(e) {
      message ("Error")
      print(e)
    },

    warning = function(w) {
      message("Warning")
      print(w)
    },

    finally = {
      message("Quitting")
      message("Bye Bye: Did you know that the moon is drifting away from Earth :)?")
    }
  )
}
