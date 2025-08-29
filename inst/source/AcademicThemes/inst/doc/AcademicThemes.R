## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(AcademicThemes)

palettes <- academic_colour_palette()
head(palettes)

## -----------------------------------------------------------------------------
library(scales)

cruk_palette <- academic_colour_palette("cruk")
cruk_palette

show_col(cruk_palette)

## -----------------------------------------------------------------------------
cruk_palette_9 <- academic_colour_palette("cruk", n = 9)
cruk_palette_9

show_col(cruk_palette_9)

## -----------------------------------------------------------------------------
library(tidyverse)

tibble(
  x = LETTERS[1:5],
  y = 5:1
) %>%
  ggplot() +
  aes(x = x, y = y, fill = x) +
  geom_col() +
  guides(fill = "none") +
  labs(
    x = "Groups",
    y = "Value"
  ) +
  theme_bw()

## -----------------------------------------------------------------------------
tibble(
  x = LETTERS[1:5],
  y = 5:1
) %>%
  ggplot() +
  aes(x = x, y = y, fill = x) +
  geom_col() +
  guides(fill = "none") +
  labs(
    x = "Groups",
    y = "Value"
  ) +
  theme_bw() +
  scale_fill_academic_d("cruk")

