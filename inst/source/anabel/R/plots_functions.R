# Copyright (C) 2025  Stefan Kraemer
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation as version 3 of the License
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.



################## Preview plot functions ##############
# Make a plot for preliminary view the data
#
# description Creates a ggplot object containing the binding curve of
# a given data frame;
#
# usage preview_plot (df, tass, tdiss, RU)
#
# details typically used to preview the data before running the analysis. The aim is
#  to aid the user deciding on the times for association and dissociation that apply best
#  to his data. Therefore it is designed to visualize at most 10 different samples/spots.
#  preview_plot() is used in the online version of anabel. See \url{http://anabel.skscience.org/anabel-1/?125791}.
#
#  seealso \code{\link{select_randomCurves}}
#
#  example select_randomCurves (df, 1, 1000, 7) %>% preview_plot(., 100, 300, "Molar)
#
#' @importFrom rlang .data

preview_plot <- function(df, tass, tdiss) {
  g <- ggplot(df, aes(.data$Time, .data$Response, col = .data$Name)) +
    geom_point() +
    geom_vline(xintercept = tass, linetype = 2) +
    geom_vline(xintercept = tdiss, linetype = 2) +
    theme_light() +
    theme(
      legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 11),
      axis.text = element_text(size = 13), axis.title = element_text(size = 14)
    )

  if (length(unique(df$Name)) < 11) {
    g <- g + scale_color_manual(values = c(
      "#A2C510", "#C61E19", "#99CFE9", "#FBB800", "#958BB2",
      "#F08000", "#6696B9", "#B2B4B5", "#6B7B88", "#5C9B5B"
    ))
  }
  return(g)
}


#' @importFrom rlang .data
preview_plot_SCK <- function(df, tass, tdiss) {
  col <- rep(c("#8ECAE6", "#438D99"), length(tass))
  col <- col[1:length(tass)]
  g <- ggplot(df, aes(.data$Time, .data$Response, col = .data$Name)) +
    geom_point() +
    geom_vline(xintercept = tass, linetype = 2, col = col) +
    geom_vline(xintercept = tdiss, linetype = 2, col = col) +
    ylab("Response") +
    theme_light() +
    theme(
      legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 11),
      axis.text = element_text(size = 13), axis.title = element_text(size = 14)
    )

  if (length(unique(df$variable)) < 5) {
    g <- g + scale_color_manual(values = c("#A2C510", "#C61E19", "#99CFE9", "#FBB800", "#958BB2"))
  }
  return(g)
}

################## Main plot output ##############

# returns the pdf size
pdf_doc_size <- function(val) {
  if (val <= 2) {
    return(c(5, 2))
  } else if (val <= 5) {
    return(c(11, 5))
  } else if (val <= 10) {
    return(c(10, 5))
  } else if (val < 20) {
    return(c(10, 11))
  } else if (val < 50) {
    return(c(14, 14))
  } else if (val < 70) {
    return(c(14, 21))
  } else if (val < 100) {
    return(c(14, 40))
  } else {
    return(c(14, 49))
  }
}

#' @importFrom rlang .data
prePlot <- function(fit_df, kntks_df, method = "SCA", outdir, quiet = TRUE) {
  if (nrow(kntks_df) == 0) {
    kntks_df <- data.frame(
      ID = unique(fit_df$ID), KD = "-",
      kass = "-", kdiss = "-", FittingQ = "Failed"
    )
  }
  . <- NULL
  kntks_df$KD <- formatC(kntks_df$KD, format = "e", digits = 2)
  kntks_df$kass <- formatC(kntks_df$kass, format = "e", digits = 2)
  kntks_df$kdiss <- formatC(kntks_df$kdiss, format = "e", digits = 2)

  ids <- unique(fit_df$ID)
  ids <- ids[!ids %in% kntks_df$ID]

  if (length(ids) > 0) kntks_df <- bind_rows(kntks_df, data.frame(ID = ids, KD = "-", kass = "-", kdiss = "-", FittingQ = "Failed"))

  # create lab for the plot
  kntks_df$Name <- gsub("^\\d+\\_", "", kntks_df$ID)
  kntks_df$lab <- paste(
    "KD:", kntks_df$KD,
    " ka:", kntks_df$kass, " kd:", kntks_df$kdiss
  )
  kntks_df$FittingQ[which(!kntks_df$FittingQ %in% c("Failed", "Warning"))] <- "Passed"

  df <- merge(fit_df[, c("ID", "Name", "Time", "Response", "fit")], kntks_df[, c("Name", "lab", "FittingQ")], by = "Name", all = TRUE)

  # do the batching thing
  create_plot_batches(df = df, outdir, quiet)
  merge_plots(outdir, method)
}

create_plot_batches <- function(df, outdir, quiet = TRUE) {
  df$ID <- gsub("^(\\d+)\\_.+", "\\1", df$ID) %>% as.numeric()
  ids <- unique(df$ID) %>% sort()

  if (length(ids) < 100) {
    n <- dplyr::case_when(
      length(ids) <= 5 ~ 2,
      length(ids) <= 10 ~ 3,
      length(ids) <= 15 ~ 4,
      TRUE ~ 5
    )

    pdf_size <- pdf_doc_size(length(ids))
    grDevices::pdf(paste0(outdir, "/samples_", min(df$ID), "-", max(df$ID), ".pdf"), width = pdf_size[1], height = pdf_size[2])
    plot_fit(fit_df = df, n = n) %>% print()
    grDevices::dev.off()
  } else {
    counter <- seq(1, length(ids), 100) %>% length()
    pb <- progress::progress_bar$new(format = "Plotting [:bar] :current/:total |:percent", total = counter, width = 50, clear = FALSE)

    for (i in seq(1, length(ids), 100)) {
      selected_ids <- ids[i:(i + 99)]
      selected_ids <- selected_ids[!is.na(selected_ids)]

      n <- dplyr::case_when(
        length(selected_ids) <= 5 ~ 2,
        length(selected_ids) <= 10 ~ 3,
        length(selected_ids) <= 15 ~ 4,
        TRUE ~ 5
      )

      pdf_size <- pdf_doc_size(length(selected_ids))
      grDevices::pdf(paste0(outdir, "/samples_", min(selected_ids), "-", max(selected_ids), ".pdf"), width = pdf_size[1], height = pdf_size[2])
      plot_fit(fit_df = df[df$ID %in% selected_ids, ], n = n) %>% print()
      grDevices::dev.off()
      if (!quiet) pb$tick()
    }
  }
}

#' @importFrom rlang .data
plot_fit <- function(fit_df, n = 5) {
  l <- fit_df[!duplicated(fit_df[, c("Name", "lab")]), c("Name", "lab")]
  txt_pos <- max(fit_df$Response) * 1.2

  p <- ggplot(fit_df, aes(x = .data$Time, group = .data$Name)) +
    geom_point(aes(y = .data$Response, col = .data$FittingQ), size = 0.5) +
    geom_text(data = l, aes(label = .data$lab, x = -Inf, y = txt_pos), hjust = -0.05, vjust = 1, size = 3) +
    theme_minimal() +
    theme(strip.background = element_rect("white"), legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = c("Passed" = "#A2C510", "Warning" = "#6696B9", "Failed" = "#A2A4A6")) +
    facet_wrap(~ .data$Name, ncol = n)

  if (any(!is.na(fit_df$fit))) p <- p + geom_path(aes(y = .data$fit), col = "#2C4255")

  return(p)
}

#' @importFrom qpdf pdf_combine
merge_plots <- function(outdir, method) {
  files <- list.files(path = outdir, pattern = "^samples_")
  x <- gsub("^samples_", "", files)
  x <- gsub("-.+$", "", x) %>% as.numeric()
  f <- data.frame(files = paste0(outdir, "/", files), ord = x)
  f <- f[order(f$ord), ]
  if (length(files) > 1) {
    qpdf::pdf_combine(f$files, paste0(outdir, "/", method, "_plots.pdf"))
    unlink(f$files)
  } else {
    files <- paste0(outdir, files)
    file.rename(files, paste0(outdir, "/", method, "_plots.pdf"))
  }
}

#' @importFrom rlang .data
plot_mck <- function(fit_df, kntks_df, outdir) {
  if (nrow(kntks_df) == 0) {
    kntks_df <- data.frame(
      KD = "-", kass = "-", kdiss = "-",
      FittingQ = "Failed"
    )
  }
  . <- NULL
  kntks_df$KD <- formatC(kntks_df$KD, format = "e", digits = 2)
  kntks_df$kass <- formatC(kntks_df$kass, format = "e", digits = 2)
  kntks_df$kdiss <- formatC(kntks_df$kdiss, format = "e", digits = 2)

  kntks_df$lab <- paste("KD:", kntks_df$KD, "\nka:", kntks_df$kass, " kd:", kntks_df$kdiss)
  df <- mutate(fit_df, lab = kntks_df$lab, FittingQ = kntks_df$FittingQ)

  p <- ggplot(df, aes(x = .data$Time, group = .data$Name)) +
    geom_point(aes(y = .data$Response, col = .data$FittingQ), size = 0.5) +
    theme_minimal() +
    theme(strip.background = element_rect("white"), legend.position = "none") +
    scale_color_manual(values = c("Passed" = "#A2C510", "Warning" = "#6696B9", "Failed" = "#A2A4A6")) +
    facet_wrap(~ .data$lab)

  if (any(!is.na(fit_df$fit))) p <- p + geom_path(aes(y = .data$fit), col = "#2C4255")

  pdf_size <- pdf_doc_size(1)
  grDevices::pdf(paste0(outdir, "/MCK_plot.pdf"), width = pdf_size[1], height = pdf_size[2])
  p %>% print()
  grDevices::dev.off()
}
