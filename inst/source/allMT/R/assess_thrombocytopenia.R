#' Assess hematological toxicities : Thrombocytopenia
#'
#'@description Evaluate number of thrombocytopenia episodes and their duration for a given patient or cohort
#'
#'@param input_files_path path to a file or a folder with MT csv files (in quotes).
#'@param plt_range Platelet (PLT) value range of c(thrombocytopenic PLT threshold, recovered PLT threshold). NOTE: Ensure that units are the same as unit of PLT in the input data.
#'@param duration_plt numeric duration (in weeks) that is used to categorize event as "long duration thrombocytopenia" (optional)
#'
#'@return Returns a list with (1) the thrombocytopenia information for each patient as listed below,
#'(2) analysis summary as dataframe (3) analysis summary as HTML table.
#'\enumerate{
#'        \item{Pat ID}
#'        \item{Number of particular toxicity episodes}
#'        \item{Duration of particular toxicity (in weeks)}
#'        \item{Number of long duration toxicity episodes}
#'        \item{Duration of long duration toxicity (in weeks)}
#'}
#'
#'@note
#'\enumerate{
#'   \item{If the function is used for cohort analysis then values are represented as median and interquartile range (IQR) (25%-75%). The median and IQR is rounded off to upper integer value if decimal value is greater or equal to 0.5, else to lower integer value. Example 1.4->1 and 3.75->4}
#'   \item{Long duration toxicity is only analyzed if "duration_plt" is included in provided arguments}
#'   \item{User may save the result as a list, if required, to analyze each patient thrombocytopenia analysis by analyzing 1st element of list}
#'}
#'
#'@include rounding_off.R
#'@seealso [assess_neutropenia()], [assess_anemia()]
#'
#'@examples
#'pat_data = system.file("extdata/processed_data/", "UPN_914.csv", package = "allMT")
#'assess_thrombocytopenia(input_files_path = pat_data,
#'                       plt_range = c(50, 75), duration_plt = 3)
#'
#'\donttest{
#'cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#'assess_thrombocytopenia(input_files_path = cohort_path,
#'                       plt_range = c(50, 75), duration_plt = 3)
#'
#'
#'result <- assess_thrombocytopenia(input_files_path = pat_data,
#'                       plt_range = c(0.5, 0.75), duration_plt = 3)
#'print(result[[1]])
#'print(result[[2]])
#'print(result[[3]])
#'}
#'
#' @export
#'
assess_thrombocytopenia<- function(input_files_path,
                                   plt_range, duration_plt = NA){

tryCatch(
    expr = {

        thrmb_df_final <- NULL
        file_name <- NULL
        result <- NULL

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


        for(z in seq(pat_list)){

          neut_df <- data.frame(matrix(ncol = 5, nrow = 1))
          x <- c("ID", "Number of episodes", "Duration (weeks)", "Number of long duration toxicity episodes",
                 "Duration of long duration toxicity")
          colnames(neut_df) <- x
          pat_csv <- pat_list[[z]]

          # print("pat_csv ready")

          if(nrow(pat_csv) >= 1){

            MT_N <- NULL
            MR <- pat_csv

            l=1
            # dur=NULL
            thrmb_dur <- data.frame(matrix(ncol = 1, nrow = 0))
            colnames(thrmb_dur) <- c("Thrmb_duration")
            flag=0
            while (l<=nrow(MR))
            {
              j=l

              flag=0
              if(MR$PLT[l]<=plt_range[1]){
                j=l
                while (j<=nrow(MR)){

                  if(MR$PLT[j]>= plt_range[2] | j==nrow(MR)){
                    d_thrmb <- MR$Weeks[j] - MR$Weeks[l]
                    thrmb_dur = as.data.frame(rbind(thrmb_dur, d_thrmb))
                    flag=1
                    jj=j
                    break
                  }
                  j=j+1
                }
              }

              if(j == nrow(MR)){
                break
              }

              if(flag=="1"){
                l=j
              }

              if(flag=="0"){
                l = l+1
              }

            }

            ########

            # print("calculations")
            colnames(thrmb_dur) <- c("Thrmb_duration")
            # neut_df <- data.frame(ID = file_name)
            neut_df$`Number of episodes` <- nrow(thrmb_dur)
            neut_df$`Duration (weeks)` <- sum(thrmb_dur$Thrmb_duration)

            # Extract all the long duration thromobocytopenia episodes
            if(!is.na(duration_plt)){
              # print("starting long dur plt")
              neut_Long_dur <- thrmb_dur%>%
                dplyr::filter(thrmb_dur$Thrmb_duration >= duration_plt)

              neut_df$`Number of long duration toxicity episodes` <- nrow(neut_Long_dur)
              neut_df$`Duration of long duration toxicity` <- sum(neut_Long_dur$Thrmb_duration)

            } else {
              neut_df$`Number of long duration toxicity episodes` <- NA
              neut_df$`Duration of long duration toxicity` <- NA
            }

          }

          if(length(pat_list) <=1){

            neut_df$ID <- tools::file_path_sans_ext(file_name)
            thrmb_df_final <- neut_df
          }

          if(length(pat_list) > 1){

            neut_df$ID <- tools::file_path_sans_ext(names(pat_list)[z])
            thrmb_df_final <- rbind(thrmb_df_final, neut_df)

          }
        }

        # Get median and IQR for the cohort, else for single patient
        if(length(pat_list) > 1){

          # neutVars <- c(names(thrmb_df_final[2:5]))
          # print(tableone::CreateTableOne(data = thrmb_df_final[neutVars]))

          temp_summary <- as.data.frame(sapply(thrmb_df_final[,-1], rounding_off))
          colnames(temp_summary)[1] <- "Result"
          temp_summary$Result <- as.character(temp_summary$Result)
          b <- rbind("Number of patients analyzed" = nrow(thrmb_df_final), temp_summary)

          b <- b %>%
            tibble::rownames_to_column("Parameter")


          result[[1]] <- thrmb_df_final
          result[[2]] <- b %>% knitr::kable(format = "rst")
          result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                            css.cell = "width: 250px", css.header = "background-color: #e6e6e6") %>%
            htmlTable::htmlTable(caption = "Table: Thrombocytopenia analysis", rnames = FALSE)
          return(result)

        }else{

          b <- as.data.frame(t(thrmb_df_final))
          colnames(b) <- "Result"
          b <- b %>%
            tibble::rownames_to_column("Parameter")


          result[[1]] <- thrmb_df_final
          result[[2]] <- b %>% knitr::kable(format = "rst")
          result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                            css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
            htmlTable::htmlTable( caption = "Table: Thrombocytopenia analysis", rnames = FALSE)

          # FOR Future - Do we need duration in median instead of total weeks?
          # \item{Proportion of all toxicity that is long duration toxicity (%)}
          return(result)
        }

        message("Anaemia analysis complete")

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
      message("Bye Bye: Did you know that Earth is revolving around the sun :)?")
    }
  )

}
