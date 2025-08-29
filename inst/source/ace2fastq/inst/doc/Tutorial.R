## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(ace2fastq)

## ------------------------------------------------------------------------
library(ace2fastq)

filename <- system.file("sampledat/1.seq.ace", package = "ace2fastq")

out_file <- ace_to_fastq(filename, target_dir = tempdir())

lines <- readLines(out_file$path)

length(lines)


## ----echo=FALSE----------------------------------------------------------
# shortened lines
n <- 40
substr(lines[1], 1, n)
substr(lines[2], 60, 60+n)
substr(lines[3], 1, 1)
substr(lines[4], 60, 60+n)


## ------------------------------------------------------------------------
library(ace2fastq)

filename <- system.file("sampledat/1.seq.ace", package = "ace2fastq")

out_file <- ace_to_fastq(filename = filename, target_dir = tempdir(), name2id = FALSE)

lines <- readLines(out_file$path)

## ----echo=FALSE----------------------------------------------------------
substr(lines[1], 1, n)

## ------------------------------------------------------------------------
library(ace2fastq)

filename <- system.file("sampledat/3.seq.ace", package = "ace2fastq")

out_file <- ace_to_fastq(filename, target_dir = tempdir())

lines <- readLines(out_file$path)

length(lines)


