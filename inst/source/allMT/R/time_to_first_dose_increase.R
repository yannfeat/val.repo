#' @title Plot the time taken for FIRST 6-Mercaptopurine (6MP) dose increase in cohort
#'
#' @description Evaluate median time (in weeks) to first 6MP dose increase for the cohort using Kaplan-Meier cumulative incidence estimator using \link[survminer]{ggsurvplot} package
#' @param input_files_path Path to folder with MT csv files (in quotes)
#' @param escalation_factor Percentage of increase from first dose to be considered as an "increased" dose.
#'
#' @return Median time (in weeks) to first 6MP dose increase plot
#'
#' @seealso [assess_increased_doses()]
#'
#' @examples
#' cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#' time_to_first_dose_increase(input_files_path = cohort_path,
#'                 escalation_factor = 10)
#'
#'
#' @importFrom survminer theme_cleantable
#' @importFrom ggplot2 theme_classic
#'
#'@export
time_to_first_dose_increase <- function(input_files_path, escalation_factor) {

  tryCatch(
    expr = {

      Pat_ID <- Cycle <- MP <- NULL

      if(missing(input_files_path)){
        stop("Please provide a folder path for the 'input_files_path' argument")
      }

      if(utils::file_test("-d", input_files_path)){

        message("NOTE: Analyzing provided input files")
        pat_list <- list.files(input_files_path)
        # folder_name <- basename(input_files_path)

      }else{
        stop("Please provide a valid folder path for the 'input_files_path' argument")
      }


      if(missing(escalation_factor)){
        stop("ERROR: Please provide escalation factor value for 'escalation_factor' argument")
      }

      DoseInc <- NULL
      # For each patient
      for(i in seq(pat_list)){

        # Reading MT csv sheet
        MR_0 <- utils::read.csv(paste0(input_files_path, pat_list[i]))

        # Creating dose increase "status" column. Setting "status" default to 1.
        MR <- MR_0 %>%
          dplyr::mutate(Pat_ID = gsub(".csv", "", pat_list[i]))%>%
          dplyr::relocate(Pat_ID, .before = Cycle)%>%
          dplyr::mutate(status = "1")

        # Identifying row with first dose increase for each patient
        MR_doseI <- data.frame(matrix(ncol = 12, nrow = 1))
        colnames(MR_doseI) <- names(MR)
        MR_doseI <- MR %>%
          dplyr::filter(MP > (MP[1] + (escalation_factor/100)*MP[1])) %>%
          dplyr::slice_head()

        # Converting status to "0" if dose was never increased beyond starting dose.
        if(nrow(MR_doseI) <1) {

          MR_doseI <- MR[nrow(MR),]
          MR_doseI <- MR_doseI %>%
            dplyr::mutate(status = "0")

        }
        # Combining data for all patients
        DoseInc <- rbind(DoseInc, MR_doseI)
      }

      # Confirming "Status" to be numeric
      DoseInc$status <- as.numeric(DoseInc$status)

      # Calculating and plotting time to dose increase
      plot1 <- function(DoseInc){

        fit_dose <- survival::survfit(survival::Surv(Weeks, status) ~ 1, data = DoseInc)

        print(fit_dose)

        plot <- survminer::ggsurvplot(fit_dose,
                                      data = DoseInc,
                                      conf.int = TRUE,
                                      cumevents = TRUE,
                                      cumevents.y.text = FALSE,
                                      cumevents.title = "Cumulative no.of patients experiencing 6MP dose increase",
                                      fontsize = 4,
                                      tables.y.text = TRUE,
                                      tables.theme = survminer::theme_cleantable(),
                                      ggtheme = ggplot2::theme_classic(),
                                      palette = c("#00e6ac", "#2E9FDF"),
                                      fun = "event",
                                      surv.median.line = "hv",
                                      xlab = "Time (weeks)",
                                      ylab = "Probability of first 6MP dose increase",
                                      break.time.by = 12,
                                      break.y.by = 0.2,
                                      xlim = c(0,100), ylim = c(0, 1.0),
                                      axes.offset = FALSE,
                                      legend.title = "",
                                      surv.plot.height = 7, tables.height = 0.15)

        print(plot)
        return(plot)
      }

      DoseInc_plot <- plot1(DoseInc = DoseInc)

      message("Time to dose increase graph created")
      # Returning graph
      return(DoseInc_plot)


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
      message("Bye Bye: Question - Is Pluto a planet? :)")
    }
  )
}
