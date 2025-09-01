#' Analyze physicians' compliance to dosing guidelines: INCREASE DOSE
#'
#' @description Evaluate number of times blood counts did not support physicians' INCREASE DOSE decision
#'
#' @param input_files_path path to a file or a folder with MT csv files (in quotes).
#' @param anc_threshold Absolute neutrophil count (ANC) value threshold above which doses should be increased. NOTE: Ensure that the threshold value is represented with same unit as of the input ANC data.
#' @param plt_threshold Platelet (PLT) value threshold above which doses should be increased. NOTE: Ensure that the threshold value is represented with same unit as of the input PLT data.
#' @param hb_threshold Hemoglobin (HB) value threshold below which doses should be increased. NOTE: Ensure that the threshold value is represented with same unit as of the input Hb data.
#' @param escalation_factor Percentage of increase from previous tolerated dose to be considered as "increased" dose.
#' @param tolerated_dose_duration Number of weeks with ANC, PLT, and Hb values consistently above threshold with same dose prescription,
#'                                 following which dose should be increased.
#'
#' @return Returns a list with (1) the 'INCREASE DOSE' analysis for each patient as listed below,
#' (2) analysis summary as dataframe (3) analysis summary as HTML table in viewer.
#' \enumerate{
#'        \item{Pat ID}
#'        \item{Number of decisions where the physician increased dose (a)}
#'        \item{Number of times blood counts did not support dose suspension (b)}
#'        \item{Discordance (%) = (\eqn{1 - (b/a)*100)}}}
#'
#' @note
#' \enumerate{
#'   \item{Atleast one of the threshold parameters (anc_threshold, plt_threshold, hb_threshold) must be provided to carry out analysis. Missing threshold parameter will not be considered.}
#'   \item{If the function is used for cohort analysis then a and b will be represented as median and interquartile range (IQR) (25%-75%). The median and IQR is rounded off to upper integer value if decimal value is greater or equal to 0.5, else to lower integer value Example 1.4->1 and 3.75->4}
#'   \item{User may save the result as a list, if required, to analyze each patient separately - use 1st element of list. Please refer to examples from \link[allMT]{assess_anemia}}
#' }
#'
#' @include rounding_off.R
#' @seealso [time_to_first_dose_increase()], [assess_reduced_doses()], [assess_stop_doses()]
#'
#' @examples
#'
#' cohort_path = paste0(system.file("extdata/processed_data/", package = "allMT"), "/")
#' assess_increased_doses(input_files_path = cohort_path, anc_threshold = 0.75,
#'                       plt_threshold = 75, hb_threshold = 8,
#'                       escalation_factor = 25, tolerated_dose_duration = 8)
#' \donttest{
#'
#' pat_data = system.file("extdata/processed_data/", "UPN_914.csv", package = "allMT")
#' assess_increased_doses (input_files_path = pat_data, anc_threshold = 0.75,
#'                       plt_threshold = 75, hb_threshold = 8,
#'                       escalation_factor = 15, tolerated_dose_duration = 6)
#'
#' assess_increased_doses(input_files_path = cohort_path,
#'                       anc_threshold = 0.7, plt_threshold = 60)
#'}
#'
#'
#'@export
assess_increased_doses  <- function(input_files_path, anc_threshold = NA, plt_threshold = NA,
                      hb_threshold = NA, escalation_factor, tolerated_dose_duration ){

  tryCatch(
    expr = {

      ANC <- MP <- MTX <- PLT <- Hb <- Esc <- NULL
      increase_doses_df <- NULL

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

      if(missing(escalation_factor)){
        stop("Error: Please provide value for 'escalation_factor' in percentage (Ex: 25% should be entered as 25)")
      }
      if(missing(tolerated_dose_duration)){
        stop("Error: Please provide value for 'tolerated_dose_duration' argument")
      }

      A_d <- NULL
      A_n <- NULL
      # A1_d <- NULL
      # A1_n <- NULL
      # A1_c <- NULL
      # A1_s <- NULL
      # A2_d <- NULL
      # A2_n <- NULL

      temp_df <- data.frame(matrix(ncol = 4, nrow = 1))
      x <- c("ID", "Total no. of times counts recommended a dose increase", "Total no. of 'DOSE INCREASE' decisions",
             "Discordance (%)")
      colnames(temp_df) <- x


      for(i in seq(pat_list)){
      #
      #   # Reading MT csv sheet for each patient
        MR <- pat_list[[i]]

        dose1 <- as.numeric(MR$MP[1])

        MR1 <- MR %>%
          dplyr::mutate(Esc = NA)

        if(code == "A"){

          if(all(which(!is.na(threshold_vector)) == "1")){

            for(x in 1:nrow(MR1)){
              # print(x)

              if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
                # print("start")
                dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
              if(any(MR$Weeks <= dif)){
              if(all(MR1$ANC[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                             :x] >= anc_threshold) &
                 all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                            :(x-1)] == MR$MP[x-1]) &
                 all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                   :x])) &
                 MR1$MP[x] >= dose1){

                # print("Y")
                MR1$Esc[x] <- "Y"
              }
             }
            }
           }
          } else if(all(which(!is.na(threshold_vector)) == "2")){
             # print("A_plt")
             for(x in 1:nrow(MR1)){
               # print(x)

             if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
               # print("start")
               dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
              if(any(MR$Weeks <= dif)){
              if(all(MR1$PLT[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                           :x] >= plt_threshold) &
               all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                          :(x-1)] == MR$MP[x-1]) &
               all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                 :x])) &
               MR1$MP[x] >= dose1){

              # print("Y")
               MR1$Esc[x] <- "Y"
              }
             }
            }
           }
          } else if(all(which(!is.na(threshold_vector)) == "3")){
              # print("A_hb")
            for(x in 1:nrow(MR1)){
              # print(x)

              if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
                # print("start")
                dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
              if(any(MR$Weeks <= dif)){
              if(all(MR1$Hb[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                            :x] >= hb_threshold) &
                  all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                             :(x-1)] == MR$MP[x-1]) &
                  all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                    :x])) &
                  MR1$MP[x] >= dose1){

                # print("Y")
                MR1$Esc[x] <- "Y"
               }
              }
              }
            }
          }

        } # end of code A

        if(code == "B"){
              # print("B")

          if(all(which(!is.na(threshold_vector)) %in% c("1", "2"))){
            # print("B_anc_plt")
            for(x in 1:nrow(MR1)){
              # print(x)

            if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
                # print("start")
              dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
            if(any(MR$Weeks <= dif)){
            if(all(MR1$ANC[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                           :x] >= anc_threshold) &
               all(MR1$PLT[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                           :x] >= plt_threshold) &
               all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                          :(x-1)] == MR$MP[x-1]) &
               all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                 :x])) &
               MR1$MP[x] >= dose1){

              # print("Y")
              MR1$Esc[x] <- "Y"
              }
             }
            }
           }
          } else if(all(which(!is.na(threshold_vector)) %in% c("1", "3"))){
                # print("B_anc_hb")
            for(x in 1:nrow(MR1)){
              # print(x)

              if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
                # print("start")
                dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
              if(any(MR$Weeks <= dif)){
              if(all(MR1$ANC[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                               :x] >= anc_threshold) &
                   all(MR1$Hb[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                               :x] >= hb_threshold) &
                   all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                              :(x-1)] == MR$MP[x-1]) &
                   all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                     :x])) &
                   MR1$MP[x] >= dose1){

                  # print("Y")
                  MR1$Esc[x] <- "Y"
                 }
                }
              }
            }
          } else if(all(which(!is.na(threshold_vector)) %in% c("2", "3"))){
                # print("B_plt_hb")
            for(x in 1:nrow(MR1)){
              # print(x)

              if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
                # print("start")
                dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
              if(any(MR$Weeks <= dif)){
              if(all(MR1$PLT[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                               :x] >= plt_threshold) &
                   all(MR1$Hb[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                             :x] >= hb_threshold) &
                   all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                              :(x-1)] == MR$MP[x-1]) &
                   all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                     :x])) &
                   MR1$MP[x] >= dose1){

                  # print("Y")
                  MR1$Esc[x] <- "Y"
                }
               }
              }
            }
          }

          } # End of Code B

        if(code == "C"){
              # print("C")
          for(x in 1:nrow(MR1)){
            # print(x)

            if(MR1$Weeks[x]>=(tolerated_dose_duration+1)){
              # print("start")
              dif <- min(MR1$Weeks[x] - tolerated_dose_duration)
            if(any(MR$Weeks <= dif)){
            if(all(MR1$ANC[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                             :x] >= anc_threshold) &
                 all(MR1$PLT[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                             :x] >= plt_threshold) &
                 all(MR1$Hb[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                             :x] >= hb_threshold) &
                 all(MR1$MP[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                            :(x-1)] == MR$MP[x-1]) &
                 all(is.na(MR1$Esc[which(abs(MR1$Weeks - dif) == min(abs(MR1$Weeks - dif)))[1]
                                   :x])) &
                 MR1$MP[x] >= dose1){

                # print("Y")
                MR1$Esc[x] <- "Y"
              }
             }
            }
          }

          }# end of code C

        MR2 <- MR1 %>%
          dplyr::mutate(Esc = ifelse(is.na(Esc), "N", Esc))

      # A: Counts indicate increase (D), and dose increased (N):

        A_a <- dplyr::filter(MR2, (Esc == "Y"))
        # print(A_a)
        A_d <- rbind(A_d, dplyr::count(A_a))

        A_b <- MR2 %>%
              dplyr::filter((Esc == "Y") &
                      MP >= dose1 &
                      MP >= (dplyr::lag(MP, n = 1) + (escalation_factor/100)*dplyr::lag(MP, n = 1)))
        # print(A_b)

        A_n <- rbind(A_n, dplyr::count(A_b))

      # # A1: Counts indicate increase (D), dose not increased (N):

      # A1_a <- dplyr::filter(MR2, (Esc == "Y"))
      # A1_d <- rbind(A1_d, dplyr::count(A1_a))
      #
      # # A1_b <- dplyr::filter(A1_a,  MP < (dplyr::lag(MP, n = 1) + (escalation_factor/100)*dplyr::lag(MP, n = 1)) | MP < dose1)
      # A1_b <- dplyr::filter(MR2, (Esc == "Y") &
      #                         (MP < (dplyr::lag(MP, n = 1) + (escalation_factor/100)*dplyr::lag(MP, n = 1)) | MP < dose1))
      # A1_n <- rbind(A1_n, dplyr::count(A1_b))

      # # dose continued instead:
      # A_c <- dplyr::filter(A1_a, MP == (dplyr::lag(MP, n = 1)) & MP > dose1)
      # A1_c <- rbind(A1_c, A_c)
      #
      # # dose stopped instead:
      # A_s <- dplyr::filter(A1_a, MP == 0)
      # A1_s <- rbind(A1_s, A_s)
      #
      #
      #   # A2: Dose increased (D), but counts do not support it(N):
      #
      #   A2_a <- dplyr::filter(MR2,
      #                         MP >= (dplyr::lag(MP, n = 1) + (escalation_factor/100)*dplyr::lag(MP, n = 1)) &
      #                           MP > dose1)
      #   A2_d <- rbind(A2_d, dplyr::count(A2_a))
      #
      #   A2_b <- dplyr::filter(A2_a, ANC , anc_threshold | PLT < plt_threshold & Esc == "N")
      #   A2_n <- rbind(A2_n, dplyr::count(A2_b))
      #

        if(length(pat_list) <=1){

          temp_df$ID <- tools::file_path_sans_ext(file_name)
          temp_df$`Total no. of times counts recommended a dose increase` <- sum(A_d)
          temp_df$`Total no. of 'DOSE INCREASE' decisions` <- sum(A_n)
          temp_df$`Discordance (%)` <- paste0(round(((1-(sum(A_n)/sum(A_d)))*100),2), "%")
        }

        if(length(pat_list) > 1){

          temp_df$ID <- tools::file_path_sans_ext(names(pat_list)[i])
          temp_df$`Total no. of times counts recommended a dose increase` <- sum(A_d)
          temp_df$`Total no. of 'DOSE INCREASE' decisions` <- sum(A_n)
          temp_df$`Discordance (%)` <- paste0(round(((1-(sum(A_n)/sum(A_d)))*100),2), "%")

        }
        increase_doses_df <- rbind(increase_doses_df, temp_df)
      } #end of for loop

      result <- NULL
      if(length(pat_list) > 1){
        med_res <- sapply(increase_doses_df[,2:3], rounding_off)
        df <- data.frame(Parameter = c("No. of patients analyzed",
                                       "Total no. of times counts recommended a dose increase",
                                       "Total no. of 'DOSE INCREASE' decisions",
                                       "Recommended 'INCREASE DOSE' decisions: median [IQR]",
                                       "No. of 'INCREASE DOSE' decisions: median [IQR]"),
                         Results = c(nrow(increase_doses_df), sum(increase_doses_df$`Total no. of times counts recommended a dose increase`),
                                     sum(increase_doses_df$`Total no. of 'DOSE INCREASE' decisions`),
                                     med_res[1], med_res[2]))

        result[[1]] <- increase_doses_df
        result[[2]] <- df %>% knitr::kable(format = "rst", caption = "'Increase dose' decision analysis for given cohort")
        result[[3]] <- df %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                           css.cell = "width: 250px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable( caption = "Table: 'INCREASE DOSE' analysis", rnames = FALSE)
        return(result)

      }else{

        b <- as.data.frame(t(increase_doses_df))
        colnames(b) <- "Result"
        b <- b %>%
          tibble::rownames_to_column("Parameter")

        result[[1]] <- increase_doses_df
        result[[2]] <- b %>% knitr::kable(format = "rst")
        result[[3]] <- b %>% htmlTable::addHtmlTableStyle(align = "lc", col.rgroup = c("white", "#e6e6e6"),
                                                          css.cell = "width: 200px", css.header = "background-color: #e6e6e6") %>%
          htmlTable::htmlTable( caption = "Table: 'INCREASE DOSE' analysis", rnames = FALSE)

        return(result)
      }

      message("'INCREASE DOSE' decisions analyzed")
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
      message("Bye Bye: Did you know that 1 million Earths can fit into the sun :)?")
    }
  )
}
