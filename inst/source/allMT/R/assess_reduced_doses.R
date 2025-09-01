#' Analyze physicians' compliance to dosing guidelines: REDUCE DOSE
#'
#' @description Evaluate number of times blood counts did not support physicians' REDUCE DOSE decision
#'
#' @param input_files_path path to a file or a folder with MT csv files (in quotes).
#' @param anc_range Absolute neutrophil count (ANC) range between which doses should be reduce. NOTE: Ensure that values are represented with same unit as of the input ANC data.
#' @param plt_range Platelet (PLT) range between which doses should be reduce. NOTE: Ensure that values are represented with same unit as of the input PLT data.
#' @param hb_range Hemoglobin (HB) range between which doses should be stopped. NOTE: Ensure that values are represented with same unit as of the input Hb data.
#' @param reduction_factor Percentage of 6MP starting dose (first visit dose) dose that will be called as "reduced" dose. Default = 50% of starting dose.
#'
#' @return Returns a list with (1) the 'REDUCE DOSE' analysis for each patient as listed below,
#' (2) analysis summary as dataframe (3) analysis summary as HTML table in viewer.
#' \enumerate{
#'        \item{Pat ID}
#'        \item{Number of decisions where the physician reduced dose (a)}
#'        \item{Number of times blood counts did not support dose reduction (b)}
#'        \item{Discordance (%) = (\eqn{(b/a)*100)}}
#' }
#'
#'#' @note
#' \enumerate{
#'   \item{Atleast one of the threshold parameters (anc_threshold, plt_threshold, hb_threshold) must be provided to carry out analysis. Missing threshold parameter will not be considered.}
#'   \item{If the function is used for cohort analysis then a and b will be represented as median and interquartile range (IQR) (25%-75%). The median and IQR is rounded off to upper integer value if decimal value is greater or equal to 0.5, else to lower integer value Example 1.4->1 and 3.75->4}
#'   \item{User may save the result as a list, if required, to analyze each patient separately - use 1st element of list. Please refer to examples from \link[allMT]{assess_anemia}}
#'}
#'
#' @include rounding_off.R
#' @seealso [assess_stop_doses()], [assess_increased_doses()]
#'
#' @examples
#' pat_data <- system.file("extdata/processed_data/", "UPN_915.csv", package = "allMT")
#' assess_reduced_doses(input_files_path = pat_data,
#'                      anc_range = c(0.5,0.75), plt_range = c(50,75),
#'                      hb_range = c(7,8), reduction_factor = 50)
#'
#' \donttest{
#' cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#' assess_reduced_doses(input_files_path = cohort_path,
#'                      anc_range = c(0.5,0.75), plt_range = c(50,75),
#'                      hb_range = c(7,8), reduction_factor = 50)
#' }
#'
#' @importFrom dplyr %>%
#' @export
#'
assess_reduced_doses  <- function(input_files_path, anc_range = NA, plt_range = NA, hb_range = NA, reduction_factor){

  tryCatch(
    expr = {

      ANC <- MP <- MTX <- PLT <- Hb <- NULL
      reduce_doses_df <- NULL

      # Check if "input_files_path" is provided or not
      if(missing(input_files_path)){
        stop("Please provide a file or folder path for the 'input_files_path' argument")
      }

      # If folder path provided, read all the files; else read single patient file; else stop
      if(utils::file_test("-d", input_files_path)){

        message("NOTE: Analyzing provided input file only")
        pat_list <- lapply(list.files(input_files_path), function(x){utils::read.csv(paste0(input_files_path, x))})
        names(pat_list) <- list.files(input_files_path)

      }else if(utils::file_test("-f", input_files_path)){

        message("NOTE: Analyzing provided input file only")
        pat_list <- list(utils::read.csv(input_files_path))
        file_name <- basename(input_files_path)

      }else{
        stop("Please provide a valid file or folder path for the 'input_files_path' argument")
      }

      # Check which range parameters are provided by the user to carry out analysis
      range_vector <- c(anc_range[1], plt_range[1], hb_range[1])
      range_vector_index <- c("anc_range", "plt_range", "hb_range")
      code <- NULL
      if(missing(anc_range) & missing(plt_range) & missing(hb_range)){
        stop("Please provide atleast one of the arguments: 'anc_range', 'plt_range', 'hb_range'")
      }else if(length(which(!is.na(range_vector))) == 1){
        code <- "A"
        message(paste0("NOTE: Analysis will be carried out with provided parameter: ", range_vector_index[which(!is.na(range_vector))]))
      }else if(length(which(!is.na(range_vector))) == 2){
        code <- "B"
        message(paste0("NOTE: Analysis will be carried out with provided parameter: ",
                       range_vector_index[which(!is.na(range_vector))][1], " and ",
                       range_vector_index[which(!is.na(range_vector))][2]))
      }else{
        code <- "C"
        message("NOTE: Analysis will be carried out with provided parameters: anc_range, plt_range and hb_range")
      }

      if(missing(reduction_factor)){
        stop("Please provide a value for the 'reduction_factor' argument. (Ex: 25% should be entered as 25)")
      }

      temp_df <- data.frame(matrix(ncol = 4, nrow = 1))
      x <- c("ID", "No. of 'dose reduce' decisions", "No. of times the counts did not support dose decision",
             "Discordance (%)")
      colnames(temp_df) <- x


      for(i in seq(pat_list)) {

        B2_n <- NULL
        B2_d <- NULL
        # B2_n_s <- NULL
        # B2_n_c <- NULL

        # Reading MT csv sheet for each patient
        MR <- pat_list[[i]]

        # Identifying starting dose (to be considered as max dose)
        max_dose <- MR$MP[1]

        # B2: dose decreased (Denominator), but the counts do not support it (Numerator)

        #Filtering rows where physician's decision IS to reduce dose (denominator)
        # This frame (B2_a) will be used as parent frame for below filters
        B2_a <- dplyr::filter(MR, (MP <= (dplyr::lag(MP, n = 1) - ((reduction_factor/100)*dplyr::lag(MP, n = 1))) & MP > 0))
        B2_d <- rbind(B2_d, dplyr::count(B2_a))
        # Filtering rows where blood counts do NOT support a reduce dose decision (numerator)
        # B2_b <- dplyr::filter(B2_a, (ANC <= anc_range[1] | ANC > anc_range[2]))%>%
        #   dplyr::filter(PLT <= plt_range[1] | PLT > plt_range[2])

        if(code == "A"){
          if(all(which(!is.na(range_vector)) == "1")){
            B2_b <- dplyr::filter(B2_a, (ANC <= anc_range[1] | ANC > anc_range[2]))
          }else if(all(which(!is.na(range_vector)) == "2")){
            B2_b <- dplyr::filter(B2_a, (PLT <= plt_range[1] | PLT > plt_range[2]))
          }else if(all(which(!is.na(range_vector)) == "3")){
            B2_b <- dplyr::filter(B2_a, (Hb <= hb_range[1] | Hb > hb_range[2]))
          }
        } else if(code == "B"){
          if(all(which(!is.na(range_vector)) %in% c("1", "2"))){
            B2_b <- dplyr::filter(B2_a, (ANC <= anc_range[1] | ANC > anc_range[2]))%>%
                dplyr::filter(PLT <= plt_range[1] | PLT > plt_range[2])
          }else if(all(which(!is.na(range_vector)) %in% c("1", "3"))){
            B2_b <- dplyr::filter(B2_a, (ANC <= anc_range[1] | ANC > anc_range[2]))%>%
              dplyr::filter(Hb <= hb_range[1] | Hb > hb_range[2])
          }else if(all(which(!is.na(range_vector)) %in% c("3", "2"))){
            B2_b <- dplyr::filter(B2_a, (Hb <= hb_range[1] | Hb > hb_range[2]))%>%
              dplyr::filter(PLT <= plt_range[1] | PLT > plt_range[2])
          }
        }else{
          B2_b <- dplyr::filter(B2_a, (ANC <= anc_range[1] | ANC > anc_range[2]))%>%
            dplyr::filter(PLT <= plt_range[1] | PLT > plt_range[2]) %>%
            dplyr::filter(Hb <= hb_range[1] | Hb > hb_range[2])
        }

        B2_n <- rbind(B2_n, dplyr::count(B2_b))

        if(length(pat_list) <=1){

          temp_df$ID <- tools::file_path_sans_ext(file_name)
          temp_df$`No. of 'dose reduce' decisions` <- sum(B2_d)
          temp_df$`No. of times the counts did not support dose decision` <- sum(B2_n)
          temp_df$`Discordance (%)` <- paste0(round(((sum(B2_n)/sum(B2_d))*100),2), "%")
        }

        if(length(pat_list) > 1){

          temp_df$ID <- tools::file_path_sans_ext(names(pat_list)[i])
          temp_df$`No. of 'dose reduce' decisions` <- sum(B2_d)
          temp_df$`No. of times the counts did not support dose decision` <- sum(B2_n)
          temp_df$`Discordance (%)` <- paste0(round(((sum(B2_n)/sum(B2_d))*100),2), "%")

        }
        reduce_doses_df <- rbind(reduce_doses_df, temp_df)
      } # end of for loop

      result <- NULL
      if(length(pat_list) > 1){
        med_res <- sapply(reduce_doses_df[,2:3], rounding_off)
        df <- data.frame(Parameter = c("No. of patients analyzed",
                                       "Total no. of 'REDUCED DOSE' decisions",
                                       "Total no. of time counts did not support dose decision",
                                       "'REDUCED DOSE' decisions: median [IQR]",
                                       "Counts did not support dose decision: median [IQR]"),
                         Results = c(nrow(reduce_doses_df), sum(reduce_doses_df$`No. of 'dose reduce' decisions`),
                                     sum(reduce_doses_df$`No. of times the counts did not support dose decision`),
                                     med_res[1], med_res[2]))

        result[[1]] <- reduce_doses_df
        result[[2]] <- df %>% knitr::kable(format = "rst", caption = "'Reduced dose' decision analysis for given cohort")
        result[[3]] <- df %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                           css.cell = "width: 250px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable(caption = "Table: 'REDUCED DOSE' analysis", rnames = FALSE)
        return(result)

      }else{

        b <- as.data.frame(t(reduce_doses_df))
        colnames(b) <- "Result"
        b <- b %>%
          tibble::rownames_to_column("Parameter")

        result[[1]] <- reduce_doses_df
        result[[2]] <- b %>% knitr::kable(format = "rst")
        result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                          css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable(caption = "Table: 'REDUCED DOSE' analysis", rnames = FALSE)

        return(result)
      }

      message("'REDUCED DOSE' decisions analyzed")
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
      message("Bye Bye: Did you know that sunset on Mars is blue :)?")
    }
  )
}


