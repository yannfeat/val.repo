#' Assess hematological toxicities: Anemia
#'
#' @description Evaluate number of anemia episodes and their duration for a given patient or cohort
#'
#' @param input_files_path path to a file or a folder with MT csv files (in quotes).
#' @param hb_range  Hemoglobin (HB) value range of c(Anemic HB threshold, recovered HB threshold). NOTE: Ensure that units are the same as unit of HB in the input data.
#' @param duration_hb numeric duration (in weeks) that is used to categorize event as "long duration anemia" (optional)
#'
#' @return Returns a list with (1) the anemia information for each patient as listed below,
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
#'   \item{Long duration toxicity is only analyzed if "duration_hb" is included in provided arguments}
#'   \item{User may save the result as a list, if required, to analyze each patient seperatly please use 1st element of list}
#'}
#'
#' @include rounding_off.R
#' @seealso [assess_neutropenia()], [assess_thrombocytopenia()]
#'
#' @examples
#'pat_data = system.file("extdata/processed_data/", "UPN_915.csv", package = "allMT")
#'assess_anemia(input_files_path = pat_data,
#'                       hb_range = c(7, 8), duration_hb = 3)
#'
#'\donttest{
#'
#'cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#'assess_anemia(input_files_path = cohort_path,
#'                       hb_range = c(7, 8), duration_hb = 3)
#'
#'result <- assess_anemia(input_files_path = pat_data,
#'                       hb_range = c(7, 8), duration_hb = 3)
#'print(result[[1]])
#'print(result[[2]])
#'print(result[[3]])
#'}
#'
#'
#'@export
#'
assess_anemia<- function(input_files_path,
                              hb_range, duration_hb = NA){

tryCatch(
expr = {

      ana_df_final <- NULL
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


      # Get duration of all the anemia episodes
      for(z in seq(pat_list)){

        ana_df <- data.frame(matrix(ncol = 5, nrow = 1))
        x <- c("ID", "Number of episodes", "Duration (weeks)", "Number of long duration toxicity episodes",
               "Duration of long duration toxicity")
        colnames(ana_df) <- x
        pat_csv <- pat_list[[z]]

        # print("pat_csv ready")

        if(nrow(pat_csv) >= 1){

          MT_N <- NULL
          MR <- pat_csv

          l=1
          # dur=NULL
          ana_dur <- data.frame(matrix(ncol = 1, nrow = 0))
          colnames(ana_dur) <- c("Ana_duration")
          flag=0
          while (l<=nrow(MR))
          {
            j=l

            flag=0
            if(MR$Hb[l]<=hb_range[1]){
              j=l
              while (j<=nrow(MR)){

                if(MR$Hb[j]>= hb_range[2] | j==nrow(MR)){
                  d_ana <- MR$Weeks[j] - MR$Weeks[l]
                  ana_dur = as.data.frame(rbind(ana_dur, d_ana))
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
          colnames(ana_dur) <- c("Ana_duration")
          # ana_df <- data.frame(ID = file_name)
          ana_df$`Number of episodes` <- nrow(ana_dur)
          ana_df$`Duration (weeks)` <- sum(ana_dur$Ana_duration)

          # Extract all the long duration anemia episodes
          if(!is.na(duration_hb)){
            # print("starting long dur hb")
            ana_Long_dur <- ana_dur%>%
              dplyr::filter(ana_dur$Ana_duration >= duration_hb)

            ana_df$`Number of long duration toxicity episodes` <- nrow(ana_Long_dur)
            ana_df$`Duration of long duration toxicity` <- sum(ana_Long_dur$Ana_duration)

          } else {
            ana_df$`Number of long duration toxicity episodes` <- NA
            ana_df$`Duration of long duration toxicity` <- NA
          }

        }

        if(length(pat_list) <=1){

          ana_df$ID <- tools::file_path_sans_ext(file_name)
          ana_df_final <- ana_df
        }

        if(length(pat_list) > 1){

          ana_df$ID <- tools::file_path_sans_ext(names(pat_list)[z])
          ana_df_final <- rbind(ana_df_final, ana_df)

        }
      } # for loop finish

      b<-NULL

      # Get median and IQR for the cohort, else for single patient
      if(length(pat_list) > 1){

        # anaVars <- c(names(ana_df_final[2:5]))
        # print(tableone::CreateTableOne(data = ana_df_final[anaVars]))

        temp_summary <- as.data.frame(sapply(ana_df_final[,-1], rounding_off))
        colnames(temp_summary)[1] <- "Result"
        temp_summary$Result <- as.character(temp_summary$Result)
        b <- rbind("Number of patients analyzed" = nrow(ana_df_final), temp_summary)

        b <- b %>%
          tibble::rownames_to_column("Parameter")

        result[[1]] <- ana_df_final
        result[[2]] <- b %>% knitr::kable(format = "rst")
        result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                          css.cell = "width: 250px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable( caption = "Table: Anemia analysis", rnames = FALSE)
        return(result)

      }else{

        b <- as.data.frame(t(ana_df_final))
        colnames(b) <- "Result"
        b <- b %>%
          tibble::rownames_to_column("Parameter")



        result[[1]] <- ana_df_final
        result[[2]] <- b %>% knitr::kable(format = "rst")
        result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                          css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable( caption = "Table: Anemia analysis", rnames = FALSE)

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
      message("Bye Bye: Did you know that sun rises in the east :)?")
    }
  )

}
