#' Get standard data structure
#'
#' @description Convert a Tata Medical Center Kolkata India (TMC) based excel workbook into a standard format for analysis
#'
#' @param inputpath_to_excelfolder Path to folder containing input excel files (in quotes)
#' @param exportpath_to_csvfolder Path to folder in which to save final output csv files (in quotes)
#' @param daily_mp_dose numeric value of DAILY 6-Mercaptopurine dose per \eqn{1m^{2}} (\eqn{mg/m^{2}}) as per the MT dosing protocol.
#'                      Default = \eqn{60mg/m^{2}} (as per ICiCLe-ALL-14 protocol)
#' @param weekly_mtx_dose numeric value of WEEKLY absolute Methotrexate dose per \eqn{1m^{2}} (\eqn{mg/m^{2}}) as per the MT dosing protocol.
#'                      Default = \eqn{20mg/m^{2}} (as per ICiCLe-ALL-14 protocol)
#' @return Folder with converted csv files
#' @description Convert a maintenance therapy excel workbook (with individual sheets per cycle) into a single csv file
#' with longitudinal data of blood count parameters, absolute doses and dose intensities of administered drugs.
#'
#' @seealso [convert_external_format()]
#' @examples
#' \donttest{
#' # As per ICiCLe-ALL-14 protocol (Reference PMID - 35101099):
#' path_to_excel = paste0(system.file("extdata/tmc_data/", package = "allMT"), "/")
#' save_path = paste0(tempdir(),"/")
#' convert_tmc_format(inputpath_to_excelfolder = path_to_excel,
#'                    exportpath_to_csvfolder = save_path,
#'                    daily_mp_dose = 60,
#'                    weekly_mtx_dose = 20)
#'
#'
#' # As per ICiCLe-ALL-14 protocol (Reference PMID - 35101099):
#' convert_tmc_format(inputpath_to_excelfolder = path_to_excel,
#'                    exportpath_to_csvfolder = save_path)
#'
#' # As per BFM protocol (Reference PMID - 15902295):
#' convert_tmc_format(inputpath_to_excelfolder = path_to_excel,
#'                    exportpath_to_csvfolder = save_path,
#'                    daily_mp_dose = 50,
#'                    weekly_mtx_dose = 20)
#'
#' # As per St Jude protocol (Reference PMID - 15902295):
#' convert_tmc_format(inputpath_to_excelfolder = path_to_excel,
#'                    exportpath_to_csvfolder = save_path,
#'                    daily_mp_dose = 75,
#'                    weekly_mtx_dose = 40)
#' }
#' @importFrom dplyr %>%
#'
#' @export
#'
convert_tmc_format <- function(inputpath_to_excelfolder, exportpath_to_csvfolder, daily_mp_dose = 60, weekly_mtx_dose = 20){

  tryCatch(
    expr = {
      # print(exportpath_to_csvfolder)
      # Checking if path provided is a folder path
      excel_folder_path <- inputpath_to_excelfolder

      if(!utils::file_test("-d", excel_folder_path)){
        stop("Please provide a folder path for the 'inputpath_to_excelfolder' argument")
      }

      message("NOTE: Using following values for 6-MP and MTX doses:\n daily_mp_dose = ", daily_mp_dose,
              " weekly_mtx_dose = ",  weekly_mtx_dose)

      # List all files in folder that will be converted
      MR_list <- list.files(path = excel_folder_path, pattern = "*.xlsx|*.xls|*.xlsm")

      # Per patient MT sheet conversion:
      for(a in seq(MR_list)){

        message(paste0("Reading ",MR_list[a]))

        # Reading MT workbook file
        if(!is.na(stringr::str_detect(MR_list[a], ".xlsx"))) {
          pat_df <- suppressWarnings({suppressMessages({rio::import_list(paste0(excel_folder_path,MR_list[a]))})})

        }

        if(!is.na(stringr::str_detect(MR_list[a], ".xls")) ){
          pat_df <- suppressWarnings({suppressMessages({rio::import_list(paste0(excel_folder_path,MR_list[a]))})})

        }

        if(!is.na(stringr::str_detect(MR_list[a], ".xlsm"))){
          sheets <- readxl::excel_sheets(paste0(excel_folder_path,MR_list[a]))
          tibble <- suppressWarnings({lapply(sheets,
                                             function(x) readxl::read_excel(paste0(excel_folder_path,MR_list[a]), sheet = x))})
          pat_df <- lapply(tibble, as.data.frame)
          names(pat_df) <- sheets
          rm(tibble)

        }

        # Extracting global patient information
        Patient_no <- pat_df[["Cycle1"]][1,4]
        # print(Patient_no)
        Height <- pat_df[["Cycle1"]][2,2]
        # print(Height)
        Weight <- pat_df[["Cycle1"]][3,2]
        # print(Weight)
        BSA <- pat_df[["Cycle1"]][4,2]
        # print(BSA)
        Start_date <- format(as.Date(as.numeric(pat_df[["Cycle1"]][5,3]),
                                     origin = '1899-12-30'), '%d/%m/%Y')
        # print(Start_date)
        max_MP <- as.numeric((pat_df[["Cycle1"]][[4,2]])*(daily_mp_dose*7))
        # print(max_MP)
        max_MTX <- as.numeric((pat_df[["Cycle1"]][[4,2]])*(weekly_mtx_dose))
        # print(max_MTX)

        Cycle <- MP <-  Dates <- MP_adj <- MTX <-NULL

        # Per cycle:
        all_data <- NULL
        for(i in seq(pat_df)){
          # Transposing data frame
          data_transposed_0 <- data.frame(t(pat_df[[i]]), stringsAsFactors = FALSE)
          data_transposed <- data_transposed_0[3:nrow(data_transposed_0), 5:13]

          # Creating new data frame with select column values
          coloumns <- data.frame(cbind( Dates = format(as.Date(as.numeric(data_transposed$X5),
                                                               origin = '1899-12-30'), '%d/%m/%Y'),
                                        Weeks = as.numeric(data_transposed$X6)+(12*(i-1)),
                                        ANC = as.numeric(data_transposed$X7),
                                        PLT = as.numeric(data_transposed$X8),
                                        Hb = as.numeric(data_transposed$X9),
                                        MP = as.numeric(data_transposed$X10),
                                        MTX = as.numeric(data_transposed$X12)))

          # Adding cycle data
          cycle_1 <- coloumns %>%
            dplyr::mutate(Cycle = i) %>%
            dplyr::relocate(Cycle, .before = Dates) %>%
            dplyr::filter(!is.na(coloumns$Dates) & !is.na(coloumns$Weeks) & !is.na(coloumns$ANC)
                          & !is.na(coloumns$MP) & !is.na(coloumns$MTX))


          # Combining data for all cycles
          all_data <- rbind(all_data, cycle_1)

        }

        # Processing and adding protocol dose-adjusted 6-MP and MTX dose intensities
        all_data1 <- all_data%>%
          dplyr::mutate_at(c(4:8), as.character) %>%
          dplyr::mutate_at(c(4:8), as.numeric) %>%
          dplyr::mutate(MP_adj = ((MP/max_MP)*100)) %>%
          dplyr::relocate(MP_adj, .after = MTX) %>%
          dplyr::mutate(MTX_adj = ((MTX/max_MTX)*100))


        # Providing backup if "save" folder path missing
        if(!missing(exportpath_to_csvfolder)) {
          SaveCSV_path <- exportpath_to_csvfolder
        } else{
          message("NOTE: Using input folder path as working directory to save processed files.")
          SaveCSV_path <- inputpath_to_excelfolder
        }

        # Writing final data frame into csv file in provided export folder
        utils::write.csv(all_data1, paste0(SaveCSV_path, gsub(".xls", "", gsub(".xlsm", "", gsub(".xlsx", "", MR_list[a]))),".csv"),
                         row.names = FALSE)

        message(paste0("Writing ", gsub(".xls", "", gsub(".xlsm", "", gsub(".xlsx", "", MR_list[a]))),".csv"))

      }
      message("Format switch complete")
      # rm(list=ls())
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
      message("Bye Bye: Did you know that there is a planet entierly made of diamonds :)?")
    }
  )

}
