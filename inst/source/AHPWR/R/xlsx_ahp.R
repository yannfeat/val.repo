#'@title build file with judment matrices
#'@name xlsx_ahp
#'@author Luciane Ferreira Alcoforado
#

#'@description  Function to buil file with judment matrices
#'
#'@param m is a matrice of pairwise comparison
#'@param file is the path to the output file.
#'@param sheet is a character string with the sheet name.
#'@param append is a logical value indicating if m should be appended to an existing file. If TRUE the file is read from disk.
#'@return Returns a xlsx document
#'
#'#m=diag(10)
#'#file1 = xlsx_ahp(m, file = "Example_1.xlsx", sheet = "M1", append = FALSE)
#'#file2=xlsx_ahp(m, file = "Example_1.xlsx", sheet = "M2", append = TRUE)
#'
#'#see file Example_1.XLSX in working directory
#'
#'@import xlsx
#'
#'
#'@export

xlsx_ahp = function(m, file, sheet, append){
  #require(xlsx)
  if(append == T){
      xlsx::write.xlsx(x=m, file=file,
      sheetName = sheet, append = TRUE)}
  else{
  xlsx::write.xlsx(x=m, file=file,
      sheetName = sheet, append = FALSE)}
}

#atualizar kit Java https://www.oracle.com/java/technologies/downloads/#jdk19-windows
#


