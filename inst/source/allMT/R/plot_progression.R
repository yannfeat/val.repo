#' @title Graphical representation of maintenance therapy data for single patient
#' @description Creates a line graph (trends) for absolute neutrophil count (ANC), 6-Mercaptopurine (6MP) and Methotrexate (MTX) doses across all visits for the given patient
#'
#' @param input_file_path Path to input csv file for the patient (in quotes)
#' @param anc_range ANC target range as per the protocol: (c(lower threshold, upper threshold)). NOTE: Ensure that units are the same as unit of ANC in the input data.
#' @param unit Choose either "million" or "billion".
#' \itemize{
#'            \item{"million" = million cells/L (x\eqn{10^{6}} cells/L or cells/\eqn{\mu}l)}
#'            \item{"billion" = billion cells/L (x\eqn{10^{9}} cells/L or x\eqn{10^{3}} cells/\eqn{\mu}l)}
#' }
#'
#' @note
#'\itemize{
#'            \item{If there is only one threshold for anc_range parameter, please specify the respective value and keep the other threshold as NA. eg : c(2000, NA)}
#'            \item{Horizontal dotted lines on the graph indicate anc_range thresholds and starting doses for 6MP and MTX as a reference (colour coded to corresponding parameter)}
#'         }
#' @return plot image
#'
#' @seealso [summarize_cycle_progression()]
#'
#' @examples
#' pat_data = system.file("extdata/processed_data/", "UPN_915.csv", package = "allMT")
#' plot_progression(input_file_path = pat_data, anc_range = c(0.75,1.5), unit = "billion")
#'
#' \donttest{
#'# As per BFM protocol (Reference PMID - 15902295):
#'plot_progression(input_file_path = "../UPN1.csv", anc_range = c(2, NA), unit = "billion")
#'
#'# As per St Jude protocol (Reference PMID - 15902295):
#'plot_progression(input_file_path = "../../UPN1.csv", anc_range = c(0.3, 1), unit = "billion")
#'}
#'
#' @importFrom ggplot2 aes element_rect
#'
#'@export
#'
plot_progression <- function(input_file_path, anc_range, unit){

  tryCatch(
    expr = {
      ANC <- MP <- MTX <- Weeks <- value <- variable <- NULL

      if(missing(input_file_path)){
        stop("Please provide a file path for the 'input_file_path' argument")
      }

      if(utils::file_test("-f", input_file_path)){

        message("NOTE: Analyzing provided input file")

      }else{
        stop("Please provide a valid file path for the 'input_file_path' argument")
      }

      if(missing(anc_range)){
        stop("Please provide a ANC range for the 'anc_range' argument")
      }

      if(missing(unit)){
        stop("Please provide a unit for the 'unit' argument")
      }

      # Reading patient csv sheet
      mt_csv_path <- input_file_path
      MR <- utils::read.csv(mt_csv_path)

      # Identifying and rounding up (to 1000) the highest ANC value to use as y axis limit and starting dose
      MP_start_dose <- MR$MP[1]
      MTX_start_dose <- MR$MTX[1]

      # Selecting columns of interest and melting dataframe together for plot
      melted <- dplyr::select(MR, Weeks, ANC, MP, MTX)
      melted_1 <- reshape2::melt(melted, id.vars = "Weeks")

      # Creating Line graph
      plot <- ggplot2::ggplot(melted_1,aes(x= as.numeric(Weeks), y= value, colour = variable, group = variable))+
        ggplot2::geom_hline(yintercept = anc_range[1], color = "grey", linetype = "dashed", size = 0.4)+
        ggplot2::geom_hline(yintercept = anc_range[2], color = "grey", linetype = "dashed", size = 0.4)+
        ggplot2::geom_hline(yintercept = MP_start_dose, color = "#ffd11a", linetype = "dashed", size = 0.4)+
        ggplot2::geom_hline(yintercept = (MTX_start_dose), colour = "#0099ff", linetype = "dashed", size = 0.4)+
        ggplot2::geom_line()+
        ggplot2::geom_point()+
        ggplot2::theme_classic()+
        ggplot2::labs(x= "Weeks",
                      y = "Parameters (log10)")+
        ggplot2::scale_x_continuous(limits = c(0,100), expand = c(0,0), breaks = scales::breaks_width(12))+
        ggplot2::theme(legend.background = ggplot2::element_rect(color = "dark grey",
                                                                 fill = "white", linetype = "solid"))+
        ggplot2::theme(legend.position = "bottom")

      # Changing scale and legend label as per "million" or "billion" unit
      if(unit == "million"){
        if(anc_range[1] < 1 |  anc_range[2] < 1){
          message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
        }
        mx <- round(max(MR$ANC), digits = -3)

        if(mx > 5000){
        plot <- plot+
          ggplot2::scale_y_continuous(trans = "log10",
                                      breaks = c(0, 25,50,100,250,500,750,1500,3000,4000,5000,mx+1000))+
          ggplot2::scale_color_manual(name = "Parameters: ", labels = c(bquote("ANC (x10"^~6~"/L)"),
                                                                        "6-MP dose", "MTX dose"), values = c("#00cc66","#ffd11a","#0099ff"))
        }

        if(mx <= 5000){
          plot <- plot+
            ggplot2::scale_y_continuous(trans = "log10",
                                        breaks = c(0, 25,50,100,250,500,750,1500,3000,4000, 5000))+
            ggplot2::scale_color_manual(name = "Parameters: ", labels = c(bquote("ANC (x10"^~6~"/L)"),
                                                                          "6-MP dose", "MTX dose"), values = c("#00cc66","#ffd11a","#0099ff"))

        }
      }

      if(unit == "billion"){
        if(anc_range[1] > 100 |  anc_range[2] > 100){
          message("Warning: Are you sure the anc_range corresponds to the selected 'unit'?")
        }
        mx <- round(max(MR$MP))

      if(mx > 500){
        plot <- plot+
          ggplot2::scale_y_continuous(trans = "log10",
                                      breaks = c(0, 0.5, 1, 2, 5, 10, 25,50,100,250,500,750,1100))+
          ggplot2::scale_color_manual(name = "Parameters: ", labels = c(bquote("ANC (x10"^~9~"/L)"),
                                                                        "6-MP dose", "MTX dose"), values = c("#00cc66","#ffd11a","#0099ff"))
      }

      if(mx <= 500){
        plot <- plot+
          ggplot2::scale_y_continuous(trans = "log10",
                                      breaks = c(0, 0.5, 1, 2, 5, 10, 25,50,100,250,500))+
          ggplot2::scale_color_manual(name = "Parameters: ", labels = c(bquote("ANC (x10"^~9~"/L)"),
                                                                        "6-MP dose", "MTX dose"), values = c("#00cc66","#ffd11a","#0099ff"))
      }
    }

      message("Maintenance therapy progression graph for the patient has been created")
      # Returning plot
      return(plot)

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
      message("Bye Bye: Did you know that Jupiter is biggest planet in our solar system :)?")
    }
  )
}
