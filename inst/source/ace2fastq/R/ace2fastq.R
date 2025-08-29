#' ace_to_fastq
#' 
#' Converts one or more contig sequences in .ace file format to .fastq format.
#' The parameter target_dir has a special value 'stdout' which will just return
#' the contigs as a list.
#'
#' @param filename .ace file
#' @param target_dir target directory or stdout
#' @param name2id use the file name as primary id or not. Default is TRUE.
#' @importFrom stringr str_ends str_trim str_replace
#' @return list
#' @author Reinhard Simon
#' @export
#'
#' @examples
#' 
#'   library(ace2fastq)
#'   filename <- system.file("sampledat/1.seq.ace", package = "ace2fastq")
#'  
#'   fileout <- ace_to_fastq(filename, target_dir = tempdir())
#' 
ace_to_fastq <- function(filename,
                         target_dir = dirname(filename),
                         name2id = TRUE) {
  stopifnot(stringr::str_ends(filename, ".ace"))
  stopifnot(file.exists(filename))
  stopifnot(target_dir == "stdout" | dir.exists(target_dir))
  stopifnot(is.logical(name2id))
  
  lines <- readLines(filename)
  
  n_of_contigs <- as.integer(stringr::str_split(lines[1], " ")[[1]][2])
  
  n_CO <- which(stringr::str_starts(lines, "CO", ))
  n_BQ <- which(stringr::str_starts(lines, "BQ"))
  
  # get end of BQ
  get_EQ <- function(BQ) {
    empty_lines <- which(lines == "")
    return(empty_lines[empty_lines > BQ][1] - 1 )
  }
  
  get_EC <- function(CO) {
    empty_lines <- which(lines == "")
    return(empty_lines[empty_lines > CO][1] - 1 )
  }
  
  get_id <- function(start) {
    id <- stringr::str_trim(lines[start])
    return(id)
  }
  
  get_seq <- function(start, end) {
    seq <- paste(lines[start:end], collapse = "") # get all sequence lines
    return(seq)
  }
  
  get_qal <- function(start, end) {
    qvls <- paste(lines[start:end], collapse = "") # get all quality lines
    qvls <- stringr::str_split(stringr::str_trim(qvls), " ") # separate
    qvls <- as.integer(qvls[[1]]) + 33
    qvls <- paste(vapply(qvls, intToUtf8, ""), collapse = "")
    return(qvls)
  }
  
  # make list for all seqs
  
  seqs <- list(n_of_contigs)
  class(seqs) <- "ace2fastq"
  filebase <- stringr::str_replace(basename(filename), ".ace", "")
  
  for (i in 1:n_of_contigs) {
    start <- n_CO[i]
    sid <- get_id(start)
    
    if (name2id) {
      sid <- paste0("@", filebase, " ", sid)
    } else {
      sid <- paste0("@", sid)
    }
    
    start <- n_CO[i] + 1
    end <- get_EC(n_CO[i])
    seq <- get_seq(start, end)
    
    start <- n_BQ[i] + 1
    end <- get_EQ(n_BQ[i])
    qal <- get_qal(start, end)
    
    fq <- character(4)
    fq[1] <- sid
    fq[2] <- seq
    fq[3] <- "+"
    fq[4] <- qal
    
    
    seqs[[i]] <- fq
    names(seqs)[i] <- sid
  }
  

  if (target_dir == "stdout") {
    return(seqs)
  } else {
    target_name <- file.path(target_dir, paste0(filebase, ".fastq"))
    writeLines(text = unlist(seqs), con = target_name)
    
    res <- list("path" = target_name)
    class(res) <- "ace2fastq"
    return(res)
  }
  
}
