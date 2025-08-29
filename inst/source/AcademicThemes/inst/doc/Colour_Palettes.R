## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message = FALSE-------------------------------------
library(AcademicThemes)

for (palette in academic_colour_palette_names()) {
  colour_palette <- academic_colour_palette(palette)
  image(1:length(colour_palette), 1, matrix(1:length(colour_palette)),
        main = paste0("Colour Palette: ", palette), xlab = "", ylab = "",
        col = colour_palette, xaxt = "n", yaxt = "n", bty = "n")
}

