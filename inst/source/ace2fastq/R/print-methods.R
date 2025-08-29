#' print.ace2fastq 
#'
#' print method for an object of class ace2fastq
#' 
#' @param x an ace2fastq object
#' @param ... other print parameters
#' @author Reinhard Simon
#' @family ace2fq_utils
#' @export
print.ace2fastq <- function(x, ...) {
  # Print in console
  if (length(grep(pattern = "fastq", x = x[[1]])) == 1) {
    cat(x[[1]])
  } else {
    cat("IDs:\n")
    cat(paste0("\n", names(x)))
  }
  
  return(invisible())
}