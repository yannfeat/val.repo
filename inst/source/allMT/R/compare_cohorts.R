#' @title Plot summarized maintenance therapy (MT) data to compare two or more cohorts
#'
#' @description Create an integrated summary graph facetted (by cohort). Graph illustrates
#'              weighted mean absolute neutrophil count (ANC) and dose information for each patient.
#'
#' @param input_files_path  Path to folder with MT csv files (in quotes)
#' @param method Choose from "M1" or "M2".
#'  \itemize{
#'            \item{"M1"} {= Comparison of cohorts that started MT therapy before or after a particular date of intervention (intervention_date).}
#'            \item{"M2"} {= Comparison of MT therapy between pre-determined groups.}
#'         }
#' @param intervention_date Only applicable if method = "M1". Provide date in yyyy-mm-dd format (in quotes)
#' @param unit Choose either "million" or "billion".
#' \itemize{
#'            \item{"million" = million cells/L (x\eqn{10^{6}} cells/L or cells/\eqn{\mu}l)}
#'            \item{"billion" = billion cells/L (x\eqn{10^{9}} cells/L or x\eqn{10^{3}} cells/\eqn{\mu}l)}
#' }
#' @param anc_range Vector with lower and upper thresholds of absolute neutrophil count target range: (c(lower threshold, upper threshold))
#'  \itemize{
#'            \item{Ensure units of anc_range and patient data (unit) match.}
#'         }
#' @param dose_intensity_threshold  numeric value of reference drug dose intensity (%).
#' @param group_data_path Only applicable for method = "M2". Path to EXCEL FILE containing "group" information.
#'                        Ensure "ID" and "Group" columns are present.
#'
#' @return Comparative summary graph
#'
#' @note
#'  \itemize{
#'          \item{If more than one chort need to be compared then only "M2" method is applicable}
#'         }
#'
#' @seealso [summarize_cohortMT()]
#'
#' @examples
#' cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#' compare_cohorts(input_files_path = cohort_path,
#'         method = "M1", intervention_date = "2020-12-01",
#'         unit = "billion", anc_range = c(0.75, 1.5), dose_intensity_threshold = 100)
#'
#' \donttest{
#' group_path = system.file("extdata/grouped_data/group_data.xlsx", package = "allMT")
#' compare_cohorts(input_files_path = cohort_path,
#'        method = "M2", group_data_path = group_path,
#'        unit = "billion", anc_range = c(0.75, 1.5), dose_intensity_threshold = 80)
#'}
#'@export
compare_cohorts <- function(input_files_path, unit, anc_range, dose_intensity_threshold,
                            method, intervention_date, group_data_path){

  tryCatch(
    expr = {

      if(missing(input_files_path)){
        stop("Please provide a folder path for the 'input_files_path' argument")
      }

      if(!utils::file_test("-d", input_files_path)){
        stop("Please provide a folder path for the 'input_files_path' argument")
      }
      # Setting default pat_list if not provided
      if(utils::file_test("-d", input_files_path)){
        message("NOTE: Including all files in input folder")
        pat_list <- list.files(input_files_path)
      }

      if(method == "M1" & missing(intervention_date)){
        stop("You have chosen the 'M1' method, please provide the intervention date ('intervention_date'), or choose the 'M2' method")
      }

      if(method == "M2" & missing(group_data_path)){
        stop("You have chosen the 'M2' method, please provide the grouping data ('group_data_path'), or choose the 'M1' method")
      }

      if(missing(unit)){
        stop("Please provide a unit for the 'unit' argument")
      }

      # Setting default anc_range if not provided
      if(missing(anc_range)){
        stop("Please provide a ANC range for the 'anc_range' argument")
      }

      if(missing(dose_intensity_threshold)){
        stop("Please provide a value for the 'dose_intensity_threshold' argument")
      }

      ANC <- Group<- N_6MP<- N_MTX<- N_antiMtb <- if_else <- wm_ANC <- wm_AntiMtb <- wt <- StartDate <-. <- NULL

      # Initializing function to plot graph
      Graph <- function(wt_all1, n, ANC_LowerBoundary, ANC_UpperBoundary, DI_threshold){

        SMI_plot <- ggplot2::ggplot(wt_all1,aes(x= wm_AntiMtb*100, y= wm_ANC))+
          ggplot2::labs(x= "Weighted mean Antimetabolite dose intensity (%)",
                        y = bquote("Weighted mean ANC (x10"^~.(n)~ "cells/L)"))+
          ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,250))+
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
          ggplot2::facet_grid(~Group)+
          ggplot2::theme(plot.margin = unit(c(0.6,0.75,0.4,0.3), "cm"),
                         panel.spacing = unit (1.5, "lines"))
      }


      MR_d <- NULL

      # Calculating summary data for each patient:
      for (a in seq(pat_list)) {

        # Reading patient MT csv sheet
        MR <- utils::read.csv(paste0(input_files_path, pat_list[a]))

        # Calculating weights
        W = NULL
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
        MR_c <- data.frame(Pat_ID = gsub(".csv", "", pat_list[a]),
                           # StartDate = as.Date(MR_b$Dates[1], format = "%d/%m/%Y"),
                           StartDate = as.Date(MR_b$Dates[1],if (grepl('^\\d+/\\d+/\\d+$',MR_b$Dates[1])) '%d/%m/%Y' else '%Y-%m-%d'),
                           wm_ANC = (sum(MR_b$wt_ANC)/sum(MR_b$wt)),
                           wm_AntiMtb = (sum(MR_b$wt_antiMtb)/sum(MR_b$wt)),
                           wm_MP = (sum(MR_b$wt_MP)/sum(MR_b$wt)),
                           wm_MTX = (sum(MR_b$wt_MTX)/sum(MR_b$wt)))

        # Combining data for all patients
        MR_d <- rbind(MR_d, MR_c)
      }


      # Analyzing and creating plot with Method 1 (intervention date provided)
      if(method == "M1"){

        if(missing(intervention_date)){
          stop("intervention_Date missing")
        }

        if(!missing(intervention_date)){

          int_date <- as.Date(intervention_date)

          # Assigning groups based on intervention date
          wt_all1 <- MR_d %>%
            dplyr::mutate(Group = ifelse(StartDate < int_date, "Pre-Intervention", "Post-Intervention"))
          wt_all1$Group <- factor(wt_all1$Group, levels = c("Pre-Intervention", "Post-Intervention"))

          # Creating graph and adjusting scales/labels as per unit provided
          if(unit == "million") {
            if(anc_range[1] < 1 |  anc_range[2] < 1){
              message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
            }

            SMI_plot <- Graph(wt_all1 = wt_all1, n = 6,  ANC_LowerBoundary = anc_range[1], ANC_UpperBoundary = anc_range[2],
                              DI_threshold = dose_intensity_threshold)
            SMI_plot <- SMI_plot+
              ggplot2::scale_y_continuous(limits = c(0,max(wt_all1$wm_ANC)+500),expand = c(0,0), breaks = scales::breaks_width(500))

          }

          if(unit == "billion") {
            if(anc_range[1] > 100 |  anc_range[2] > 100){
              message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
            }

            SMI_plot <- Graph(wt_all1 = wt_all1, n = 9,  ANC_LowerBoundary = anc_range[1], ANC_UpperBoundary = anc_range[2],
                              DI_threshold = dose_intensity_threshold)
            SMI_plot <- SMI_plot+
              ggplot2::scale_y_continuous(limits = c(0,max(wt_all1$wm_ANC)+0.5),expand = c(0,0), breaks = scales::breaks_width(0.5))

          }

          # Returning plot
          message("Comparative summary graph created")
          return(SMI_plot)
        }

      }

      # Analyzing and creating plot with Method 2 (pre-determined groups)
      if(method == "M2"){

        if(missing(group_data_path)){
          stop("'group_data_path' missing")
        }
        # Reading file with group information
        Group_data = readxl::read_excel(group_data_path)

        if(all(!names(Group_data) %in% c("Pat_ID", "Group"))){
          stop("Please ensure the ID column and grouping variable columns are names 'Pat_ID' and 'Group' respectively")
        }

        # Joining group data with computed summary data for patients in cohort
        if(any(names(Group_data) %in% c("Pat_ID"))){
          wt_all1 <- MR_d %>%
            dplyr::left_join(., Group_data)

        }

        # Creating graph and adjusting scales/labels as per unit provided
        if(unit == "million") {
          if(anc_range[1] < 1 |  anc_range[2] < 1){
            message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
          }

          SMI_plot <- Graph(wt_all1 = wt_all1, n = 6,  ANC_LowerBoundary = anc_range[1], ANC_UpperBoundary = anc_range[2],
                            DI_threshold = dose_intensity_threshold)
          SMI_plot <- SMI_plot+
            ggplot2::scale_y_continuous(limits = c(0,max(wt_all1$wm_ANC)+500),expand = c(0,0), breaks = scales::breaks_width(500))

        }
        if(unit == "billion") {

          if(anc_range[1] > 100 |  anc_range[2] > 100){
            message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
          }

          SMI_plot <- Graph(wt_all1 = wt_all1, n = 9,   ANC_LowerBoundary = anc_range[1], ANC_UpperBoundary = anc_range[2],
                            DI_threshold = dose_intensity_threshold)
          SMI_plot <- SMI_plot+
            ggplot2::scale_y_continuous(limits = c(0,max(wt_all1$wm_ANC)+0.5),expand = c(0,0), breaks = scales::breaks_width(0.5))


        }

        # Returning plot
        message("Comparative summary graph created")
        return(SMI_plot)

      }

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
      message("Bye Bye: Did you know that space is silent :)?")
    }
  )
}
