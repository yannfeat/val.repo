#' `summary` method for class "`msabe`"
#' 
#' @param object an output from `msabe`
#' @param ... additional arguments for print
#' @return Does not return anything; print a summary of the output
#' @method summary msabe
#' @export
summary.msabe <- function(object, ...) {
    digits <- max(3, getOption("digits") - 3)
    cat("Mixed criterion results\n")
    cat("---------------------------------------------------\n")
    cat("Bioequivalent?")
    cat(paste0("\t", ifelse(object$fout[1] > 0.0, "Yes", "No")))
    cat(paste0("\t(S_{WR} = ", round(sqrt(object$fout[3]), digits = digits), ")\n"))
    cat("TOST vs. RSABE?")
    cat(paste0("\t", ifelse(sqrt(object$fout[3]) > 0.249, "RSABE", "TOST"), "\n"))
    cat(paste0("Point estimate (GMR):\t", round(exp(object$fout[2]), digits = digits)), "\n")
    cat("Confidence Interval:")
    cat(paste0("\t[", ifelse(sqrt(object$fout[3]) > 0.249, "-Inf", round(exp(object$fout[5]), digits = digits)), ", ", ifelse(sqrt(object$fout[3]) > 0.294, round(object$fout[6], digits = digits), round(exp(object$fout[6]), digits = digits)), "]\n"))
}