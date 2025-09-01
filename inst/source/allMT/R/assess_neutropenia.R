#' Assess hematological toxicities: Neutropenia
#'
#' @description Evaluate number of neutropenia episodes and their duration for a given patient or cohort
#'
#' @param input_files_path path to a file or a folder with MT csv files (in quotes).
#' @param anc_range Absolute neutrophil count (ANC) value range of c(Neutropenic ANC threshold, recovered ANC threshold). NOTE: Ensure that units are the same as unit of ANC in the input data.
#' @param duration_anc numeric duration (in weeks) that is used to categorize event as "long duration neutropenia" (optional)
#'
#' @return Returns a list with (1) the neutropenia information for each patient as listed below,
#' (2) analysis summary as dataframe (3) analysis summary as HTML table.
#' \enumerate{
#'        \item{Pat ID}
#'        \item{Number of particular toxicity episodes}
#'        \item{Duration of particular toxicity (in weeks)}
#'        \item{Number of long duration toxicity episodes}
#'        \item{Duration of long duration toxicity (in weeks)}
#' }
#'
#' @note
#' \enumerate{
#'   \item{If the function is used for cohort analysis then values are represented as median and interquartile range (IQR) (25%-75%). The median and IQR is rounded off to upper integer value if decimal value is greater or equal to 0.5, else to lower integer value. Example 1.4->1 and 3.75->4}
#'   \item{Long duration toxicity is only analyzed if "duration_anc" is included in provided arguments}
#'   \item{User may save the result as a list, if required, to analyze each patient neutropenia analysis by analyzing 1st element of list}
#'}
#'
#' @include rounding_off.R
#' @seealso [assess_anemia()], [assess_thrombocytopenia()]
#'
#' @examples
#'pat_data = system.file("extdata/processed_data/", "UPN_915.csv", package = "allMT")
#'assess_neutropenia(input_files_path = pat_data,
#'                       anc_range = c(0.5, 0.75), duration_anc = 3)
#'
#'\donttest{
#'
#'cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#'assess_neutropenia(input_files_path = cohort_path,
#'                       anc_range = c(0.5, 0.75), duration_anc = 3)
#'
#'
#'result <- assess_neutropenia(input_files_path = pat_data,
#'                       anc_range = c(0.5, 0.75), duration_anc = 3)
#'print(result[[1]])
#'print(result[[2]])
#'print(result[[3]])
#'}
#'
#'@export
#'
assess_neutropenia<- function(input_files_path,
                              anc_range, duration_anc = NA){
tryCatch(
    expr = {

        neut_df_final <- NULL
        file_name <- NULL
        result <- NULL

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

        # Get duration of all the neutropenia episodes
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
            neut_dur <- data.frame(matrix(ncol = 1, nrow = 0))
            colnames(neut_dur) <- c("Neut_duration")
            flag=0
            while (l<=nrow(MR))
            {
              j=l

              flag=0
              if(MR$ANC[l]<=anc_range[1]){
                j=l
                while (j<=nrow(MR)){

                  if(MR$ANC[j]>= anc_range[2] | j==nrow(MR)){
                    d_neut <- MR$Weeks[j] - MR$Weeks[l]
                    neut_dur = as.data.frame(rbind(neut_dur, d_neut))
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
            colnames(neut_dur) <- c("Neut_duration")
            # neut_df <- data.frame(ID = file_name)
            neut_df$`Number of episodes` <- nrow(neut_dur)
            neut_df$`Duration (weeks)` <- sum(neut_dur$Neut_duration)

            # Extract all the long duration neutropenia episodes
            if(!is.na(duration_anc)){
              # print("starting long dur anc")
              neut_Long_dur <- neut_dur%>%
                dplyr::filter(neut_dur$Neut_duration >= duration_anc)

              neut_df$`Number of long duration toxicity episodes` <- nrow(neut_Long_dur)
              neut_df$`Duration of long duration toxicity` <- sum(neut_Long_dur$Neut_duration)

            } else {
              neut_df$`Number of long duration toxicity episodes` <- NA
              neut_df$`Duration of long duration toxicity` <- NA
            }

          }

          if(length(pat_list) <=1){

            neut_df$ID <- tools::file_path_sans_ext(file_name)
            neut_df_final <- neut_df
          }

          if(length(pat_list) > 1){

            neut_df$ID <- tools::file_path_sans_ext(names(pat_list)[z])
            neut_df_final <- rbind(neut_df_final, neut_df)

          }
        } # for loop finish

        b<-NULL

        # Get median and IQR for the cohort, else for single patient
        if(length(pat_list) > 1){

          # neutVars <- c(names(neut_df_final[2:5]))
          # print(tableone::CreateTableOne(data = neut_df_final[neutVars]))

          temp_summary <- as.data.frame(sapply(neut_df_final[,-1], rounding_off))
          colnames(temp_summary)[1] <- "Result"
          temp_summary$Result <- as.character(temp_summary$Result)
          b <- rbind("Number of patients analyzed" = nrow(neut_df_final), temp_summary)

          b <- b %>%
            tibble::rownames_to_column("Parameter")
          # b$Parameter[1] <- "Number of patients analyzed"

          # print(b)

          result[[1]] <- neut_df_final
          result[[2]] <- b %>% knitr::kable(format = "rst")
          result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                              css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
                               htmlTable::htmlTable(caption = "Table: Neutropenia analysis", rnames = FALSE)
          return(result)

        }else{

          b <- as.data.frame(t(neut_df_final))
          colnames(b) <- "Result"
          b <- b %>%
            tibble::rownames_to_column("Parameter")


          result[[1]] <- neut_df_final
          result[[2]] <- b %>% knitr::kable(format = "rst")
          result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                              css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
                               htmlTable::htmlTable(caption = "Table: Neutropenia analysis", rnames = FALSE)

          # FOR Future - Do we need duration in median instead of total weeks?
          # \item{Proportion of all toxicity that is long duration toxicity (%)}
          return(result)
        }
        message("Neutropenia analysis complete")

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
      message("Bye Bye: Did you know that Earth is spherical in shape :)?")
    }
  )

}
