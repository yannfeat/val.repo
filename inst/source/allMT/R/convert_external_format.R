#' Get standard data structure
#'
#' @description Convert a maintenance therapy excel sheet created by user into a standard format for analysis
#'
#' @param inputpath_to_excelfolder Path to folder containing input excel files (in quotes)
#' @param exportpath_to_csvfolder Path to folder in which to save final output csv files (in quotes)
#' @param pat_data_file_path Path to excel file with patient IDs and corresponding BSA (body surface area) values.
#'                           Column names - "ID", "BSA".
#' @param daily_mp_dose numeric value of DAILY 6-Mercaptopurine dose per \eqn{1m^{2}} (\eqn{mg/m^{2}}) as per the MT dosing protocol.
#'                      Default = \eqn{60mg/m^{2}} (as per ICiCLe-ALL-14 protocol)
#' @param weekly_mtx_dose numeric value of WEEKLY absolute Methotrexate dose per \eqn{1m^{2}} (\eqn{mg/m^{2}}) as per the MT dosing protocol.
#'                      Default = \eqn{20mg/m^{2}} (as per ICiCLe-ALL-14 protocol)
#'
#' @note If MT excel files are missing in the input folder for any patient IDs provided in the pat_data_file ID column,
#'       the function will show a message of missing IDs but will continue to convert the available files.
#'
#' @return Folder with converted csv files
#'
#' @seealso [convert_tmc_format()]
#'
#' @examples
#' # As per ICiCLe-ALL-14 protocol (Reference PMID - 35101099):
#' path_to_excel = paste0(system.file("extdata/external_data/", package = "allMT"), "/")
#' save_path = paste0(tempdir(),"/")
#' path_to_bsa = system.file("extdata/external_data/", "BSA.xlsx", package = "allMT")
#' convert_external_format(inputpath_to_excelfolder = path_to_excel,
#'                    exportpath_to_csvfolder = save_path,
#'                    pat_data_file_path = path_to_bsa,
#'                    daily_mp_dose = 60,
#'                    weekly_mtx_dose = 20)
#' \donttest{
#' # As per ICiCLe-ALL-14 protocol (Reference PMID - 35101099):
#' convert_external_format(inputpath_to_excelfolder = "../csv_trial/",
#'                    exportpath_to_csvfolder = save_path,
#'                    pat_data_file_path = "BSAFile.xlsx")
#'
#' # As per BFM protocol (Reference PMID - 15902295):
#' convert_external_format(inputpath_to_excelfolder = "../csv_trial/",
#'                    exportpath_to_csvfolder = save_path,
#'                    pat_data_file_path = "BSAFile.xlsx",
#'                    daily_mp_dose = 50,
#'                    weekly_mtx_dose = 20)
#'
#' # As per St Jude protocol (Reference PMID - 15902295):
#' convert_external_format(inputpath_to_excelfolder = "../csv_trial/",
#'                    exportpath_to_csvfolder = save_path,
#'                    pat_data_file_path = "BSAFile.xlsx",
#'                    daily_mp_dose = 75,
#'                    weekly_mtx_dose = 40)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
convert_external_format <- function(inputpath_to_excelfolder, exportpath_to_csvfolder,
                                    pat_data_file_path, daily_mp_dose = 60, weekly_mtx_dose = 20){

  tryCatch(
    expr = {
      Cycle <- Dates <- MP <- MP_adj <- MTX <- Weeks <- NULL

      BSA_file_Path <- pat_data_file_path

      message("NOTE: Using following values for 6-MP and MTX doses:\n daily_mp_dose = ", daily_mp_dose,
              " weekly_mtx_dose = ",  weekly_mtx_dose)

      # Reading Pat_data_file (BSA_file)
      if(stringr::str_detect(BSA_file_Path, ".xlsx|.xls|.xlsm")){
        BSA_file <- readxl::read_excel(path = BSA_file_Path)
      }

      if(stringr::str_detect(BSA_file_Path, ".csv")){
        BSA_file <- utils::read.csv(path = BSA_file_Path)
      }

      if(!all(c("ID", "BSA") %in% colnames(BSA_file))){
        stop("pat_data_file must include column names 'ID', 'BSA'")}

      if(all(c("ID", "BSA") %in% colnames(BSA_file))){

        # Identifying patients to be analysed from Pat_data/BSA file
        patients <- BSA_file$ID

        # Checking if path provided is a folder path
        if(!utils::file_test("-d", inputpath_to_excelfolder)){
          stop("Please provide a folder for the 'inputpath_to_excelfolder' argument")
        }

        # Listing all MT excel/csv sheets in provided input folder
        FolderFiles <- list.files(inputpath_to_excelfolder)

        # Listing all MT excel/csv sheets that match patient ID from pat_data/BSA file.
        # This is the list of sheets that will be converted
        patient_list <- FolderFiles[tools::file_path_sans_ext(FolderFiles) %in% patients]

        # Warning of any missing MT sheets from the patient ID list provided in pat_data/BSA file
        if(any(!patients %in% tools::file_path_sans_ext(FolderFiles))){
          message(paste0("NOTE: Following patient MT data files are missing: ",
                         patients[!patients %in% tools::file_path_sans_ext(FolderFiles)]))
        }

        # Per patient MT sheet conversion:
        csv <- NULL
        for(a in seq(patient_list)){

          message(paste0("Reading ",patient_list[a]))

          # Reading MT excel/csv sheet
          if(stringr::str_detect(patient_list[a], ".xlsx|.xls")) {
            pat_df <- readxl::read_excel(paste0(inputpath_to_excelfolder, patient_list[a]))
          }

          if(stringr::str_detect(patient_list[a], ".csv")) {
            pat_df <- utils::read.csv(paste0(inputpath_to_excelfolder, patient_list[a]))
          }

          if(!all(c("Dates","ANC", "PLT", "Hb", "MP", "MTX") %in% colnames(pat_df))) {
            stop("Column names of input excel must include 'Dates', 'ANC', 'PLT', 'Hb', 'MP', 'MTX'")}

          if(all(c("Dates","ANC", "PLT",  "Hb", "MP", "MTX") %in% colnames(pat_df))) {

            # Extracting global patient information
            id <- as.character(tools::file_path_sans_ext(patient_list[a]))
            bsa_value <- as.numeric(BSA_file[stringr::str_detect(BSA_file$ID, id), "BSA"])
            start_date <- as.Date(pat_df$Dates[1], format = "%d/%m/%Y")
            max_MP <- as.numeric(bsa_value*(daily_mp_dose*7))
            max_MTX <- as.numeric(bsa_value*(weekly_mtx_dose))

            # Creating new data frame with select column values
            csv <- data.frame(cbind(Dates = as.character(pat_df$Dates),
                                    ANC = as.numeric(pat_df$ANC),
                                    PLT = as.numeric(pat_df$PLT),
                                    Hb = as.numeric(pat_df$Hb),
                                    MP = as.numeric(pat_df$MP),
                                    MTX = as.numeric(pat_df$MTX)))

            # Processing and adding cycle data and protocol dose-adjusted 6-MP and MTX dose intensities
            csv1 <- csv %>%
              dplyr::mutate_at("Dates", function(x) {as.Date(x, format = '%d/%m/%Y')}) %>%
              dplyr::mutate(Weeks = as.numeric(Dates+7 - start_date)/7) %>%
              dplyr::relocate(Weeks, .after = Dates) %>%
              dplyr::mutate(Cycle = ceiling(Weeks/12)) %>%
              dplyr::relocate(Cycle, .before = Dates) %>%
              dplyr::mutate_at(c("MP", "MTX"), as.character) %>%
              dplyr::mutate_at(c("MP", "MTX"), as.numeric) %>%
              dplyr::mutate(MP_adj = (as.numeric(MP)/max_MP)*100) %>%
              dplyr::relocate(MP_adj, .after = MTX) %>%
              dplyr::mutate(MTX_adj = (as.numeric(MTX)/max_MTX)*100) %>%
              dplyr::mutate_at(Dates, as.numeric)

            if(any(is.na(csv1$Dates))){
              stop("Date format error. Ensure dates are in dd/mm/YYYY character format")
            }


            # Providing backup if "save" folder path missing
            if(missing(exportpath_to_csvfolder)){
              message("NOTE: Using input folder path as working directory to save processed files")
              exportpath_to_csvfolder <- inputpath_to_excelfolder
            }

            # Writing final data frame into csv file in provided export folder
            utils::write.csv(csv1, paste0(exportpath_to_csvfolder, id,".csv"), row.names = FALSE)

            message(paste0("Writing ", id,".csv"))


          }
          message("Format switch complete")
        }
        # rm(list=ls())
      }
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
      message("Bye Bye: Did you know that Uranus spins sideways :)?")
    }
  )

}
