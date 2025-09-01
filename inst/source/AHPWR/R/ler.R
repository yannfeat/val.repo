#'@title Read an excel file containing the paired matrices and turn all your spreadsheets into a list of matrices in R
#'@name ler
#'@author Lyncoln Oliveira
#'
#'@description  Function to Read an excel file containing the paired matrices and turn all your spreadsheets into a list of matrices in Re
#'
#'
#'@param caminho Address to an excel file that contains the worksheets
#'
#'@return Returns a list containing the paired arrays from the excel file
#'
#'@examples
#'caminho  <- system.file("tests", "test_import.xlsx", package = "xlsx")
#'lista = ler(caminho)
#'
#'@import readxl
#'
#'@export

ler = function(caminho){
  #require(readxl)
  planilhas = readxl::excel_sheets(caminho)
  (system.file("exdata", caminho,
                              package = 'AHP'))
  matrizes = suppressMessages(lapply(planilhas ,function(x) readxl::read_excel(path = caminho, col_names = FALSE, sheet = x)))
  names(matrizes) = planilhas
  return(matrizes)
}
