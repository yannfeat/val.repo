#' Analyze physicians' compliance to dosing guidelines: STOP DOSE
#'
#' @description Evaluate number of times blood counts did not support physicians' STOP DOSE decision
#'
#' @param input_files_path path to a file or a folder with MT csv files (in quotes).
#' @param anc_threshold Absolute neutrophil count (ANC) value threshold below which doses should be stopped. NOTE: Ensure that the threshold value is represented with same unit as of the input ANC data.
#' @param plt_threshold Platelet (PLT) value threshold below which doses should be stopped. NOTE: Ensure that the threshold value is represented with same unit as of the input PLT data.
#' @param hb_threshold Hemoglobin (HB) value threshold below which doses should be stopped. NOTE: Ensure that the threshold value is represented with same unit as of the input Hb data.
#'
#' @return Returns a list with (1) the 'STOP DOSE' analysis for each patient as listed below,
#' (2) analysis summary as dataframe (3) analysis summary as HTML table in viewer.
#' \enumerate{
#'        \item{Pat ID}
#'        \item{Number of decisions where the physician stopped dose (a)}
#'        \item{Number of times blood counts did not support dose suspension (b)}
#'        \item{Discordance (%) = (\eqn{(b/a)*100)}}
#' }
#'
#' @note
#' \enumerate{
#'   \item{Atleast one of the threshold parameters (anc_threshold, plt_threshold, hb_threshold) must be provided to carry out analysis. Missing threshold parameter will not be considered.}
#'   \item{If the function is used for cohort analysis then a and b will be represented as median and interquartile range (IQR) (25%-75%). The median and IQR is rounded off to upper integer value if decimal value is greater or equal to 0.5, else to lower integer value Example 1.4->1 and 3.75->4}
#'   \item{User may save the result as a list, if required, to analyze each patient separately - use 1st element of list. Please refer to examples from \link[allMT]{assess_anemia}}
#'}
#'
#' @include rounding_off.R
#' @seealso [assess_reduced_doses()], [assess_increased_doses()]
#'
#' @examples
#' pat_data <- system.file("extdata/processed_data/", "UPN_915.csv", package = "allMT")
#' assess_stop_doses(input_files_path = pat_data,
#'                   anc_threshold = 0.5, plt_threshold = 50, hb_threshold = 7)
#'
#' \donttest{
#' assess_stop_doses(input_files_path = pat_data,
#'                   anc_threshold = 0.5)
#'
#' cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#' assess_stop_doses(input_files_path = cohort_path,
#'                   anc_threshold = 0.5,plt_threshold = 50, hb_threshold = 7)
#' }
#' @importFrom dplyr %>%
#' @export
#'
assess_stop_doses  <- function(input_files_path, anc_threshold = NA, plt_threshold = NA, hb_threshold = NA){

  tryCatch(
    expr = {

      ANC <- MP <- MTX <- PLT <- Hb <- NULL
      stop_doses_df <- NULL

      # Check if "input_files_path" is provided or not
      if(missing(input_files_path)){
        stop("Please provide a file or folder path for the 'input_files_path' argument")
      }

      # If folder path provided, read all the files; else read single patient file; else stop
      if(utils::file_test("-d", input_files_path)){

        message("NOTE: Including all files in input folder")
        pat_list <- lapply(list.files(input_files_path), function(x){utils::read.csv(paste0(input_files_path, x))})
        names(pat_list) <- list.files(input_files_path)

      }else if(utils::file_test("-f", input_files_path)){

        message("NOTE: Analyzing provided input file only")
        pat_list <- list(utils::read.csv(input_files_path))
        file_name <- basename(input_files_path)

      }else{
        stop("Please provide a valid file or folder path for the 'input_files_path' argument")
      }

      # Check which thresholds are provided by the user to carry out analysis
      threshold_vector <- c(anc_threshold, plt_threshold, hb_threshold)
      threshold_vector_index <- c("anc_threshold", "plt_threshold", "hb_threshold")
      code <- NULL
      if(missing(anc_threshold) & missing(plt_threshold) & missing(hb_threshold)){
        stop("Please provide atleast one of the arguments: 'anc_threshold', 'plt_threshold', 'hb_threshold'")
      }else if(length(which(!is.na(threshold_vector))) == 1){
        code <- "A"
        message(paste0("NOTE: Analysis will be carried out with provided parameter: ", threshold_vector_index[which(!is.na(threshold_vector))]))
      }else if(length(which(!is.na(threshold_vector))) == 2){
        code <- "B"
        message(paste0("NOTE: Analysis will be carried out with provided parameter: ",
                       threshold_vector_index[which(!is.na(threshold_vector))][1], " and ",
                       threshold_vector_index[which(!is.na(threshold_vector))][2]))
      }else{
        code <- "C"
        message("NOTE: Analysis will be carried out with provided parameters: anc_threshold, plt_threshold and hb_threshold")
      }

      temp_df <- data.frame(matrix(ncol = 4, nrow = 1))
      x <- c("ID", "No. of 'dose stop' decisions", "No. of times the counts did not support dose decision",
             "Discordance (%)")
      colnames(temp_df) <- x

      for(i in seq(pat_list)){

        A2_d <- NULL
        A2_n <- NULL

        # Reading MT csv sheet for each patient
        MR <- pat_list[[i]]

        ### 2: counts are high, but doses stopped:

        # Filtering for all rows with stop dose decision (denominator)
        A2_a <- dplyr::filter(MR, MP==0 & MTX==0)
        A2_d <- rbind(A2_d, dplyr::count(A2_a))
        # FROM THE ABOVE DF, filtering for all rows where counts are above threshold - ie: counts not support decision (numerator)
        # A2_b <- dplyr::filter(A2_a, ANC > anc_threshold & PLT > plt_threshold)
          if(code == "A"){
            if(all(which(!is.na(threshold_vector)) == "1")){
              A2_b <- dplyr::filter(A2_a, ANC > anc_threshold)
            }else if(all(which(!is.na(threshold_vector)) == "2")){
              A2_b <- dplyr::filter(A2_a, PLT > plt_threshold)
            }else if(all(which(!is.na(threshold_vector)) == "3")){
              A2_b <- dplyr::filter(A2_a, Hb > hb_threshold)
            }
          } else if(code == "B"){
            if(all(which(!is.na(threshold_vector)) %in% c("1", "2"))){
              A2_b <- dplyr::filter(A2_a, ANC > anc_threshold & PLT > plt_threshold)
            }else if(all(which(!is.na(threshold_vector)) %in% c("1", "3"))){
              A2_b <- dplyr::filter(A2_a, ANC > anc_threshold & Hb > hb_threshold)
            }else if(all(which(!is.na(threshold_vector)) %in% c("3", "2"))){
              A2_b <- dplyr::filter(A2_a, Hb > hb_threshold & PLT > plt_threshold)
            }
          }else{
            A2_b <- dplyr::filter(A2_a, ANC > anc_threshold & PLT > plt_threshold & Hb > hb_threshold)
          }
        A2_n <- rbind(A2_n, dplyr::count(A2_b))

        if(length(pat_list) <=1){

          temp_df$ID <- tools::file_path_sans_ext(file_name)
          temp_df$`No. of 'dose stop' decisions` <- sum(A2_d)
          temp_df$`No. of times the counts did not support dose decision` <- sum(A2_n)
          temp_df$`Discordance (%)` <- paste0(round(((sum(A2_n)/sum(A2_d))*100),2), "%")
        }

        if(length(pat_list) > 1){

          temp_df$ID <- tools::file_path_sans_ext(names(pat_list)[i])
          temp_df$`No. of 'dose stop' decisions` <- sum(A2_d)
          temp_df$`No. of times the counts did not support dose decision` <- sum(A2_n)
          temp_df$`Discordance (%)` <- paste0(round(((sum(A2_n)/sum(A2_d))*100),2), "%")

        }
        stop_doses_df <- rbind(stop_doses_df, temp_df)
      } # end of for loop

      result <- NULL
      if(length(pat_list) > 1){
        med_res <- sapply(stop_doses_df[,2:3], rounding_off)
        df <- data.frame(Parameter = c("No. of patients analyzed",
                                       "Total no. of 'STOP DOSE' decisions",
                                       "Total no. of time counts did not support dose decision",
                                       "'STOP DOSE' decisions: median [IQR]",
                                       "Counts did not support dose decision: median [IQR]"),
                         Results = c(nrow(stop_doses_df), sum(stop_doses_df$`No. of 'dose stop' decisions`),
                                     sum(stop_doses_df$`No. of times the counts did not support dose decision`),
                                     med_res[1], med_res[2]))

        result[[1]] <- stop_doses_df
        result[[2]] <- df %>% knitr::kable(format = "rst", caption = "'Stop dose' decision analysis for given cohort")
        result[[3]] <- df %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                          css.cell = "width: 250px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable( caption = "Table: 'STOP DOSE' analysis", rnames = FALSE)
        return(result)

      }else{

        b <- as.data.frame(t(stop_doses_df))
        colnames(b) <- "Result"
        b <- b %>%
          tibble::rownames_to_column("Parameter")

        result[[1]] <- stop_doses_df
        result[[2]] <- b %>% knitr::kable(format = "rst")
        result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                          css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable( caption = "Table: 'STOP DOSE' analysis", rnames = FALSE)

        return(result)
     }

      message("'STOP DOSE' decisions analyzed")
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
      message("Bye Bye: Did you know that Earth is 3rd planet from sun :)?")
    }
  )
}
