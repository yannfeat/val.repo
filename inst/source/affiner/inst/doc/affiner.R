## ----hidden, echo = FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.cap = "", dev = "ragg_png")

## ----hex-logo2, fig.width = 4.5, fig.height = 4.5, fig.alt = "Isometric-cube hex logo", eval = requireNamespace("ggplot2", quietly=TRUE) && requireNamespace("aRtsy", quietly=TRUE) && requireNamespace("gtable", quietly=TRUE), message = FALSE----
library("aRtsy")
library("ggplot2")

gg <- canvas_planet(colorPalette("lava"), threshold = 3) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0))
grob <- ggplotGrob(gg)
grob <- gtable::gtable_filter(grob, "panel") # grab just the panel
affiner::grid.isocube(top = grob, left = grob, right = grob,
  gp_border = grid::gpar(col = "darkorange", lwd = 12))

## ----hex-logo, fig.width = 4.5, fig.height = 4.5, fig.alt = "Isometric-cube hex logo"----
library("affiner")
library("grid")

xy <- as_coord2d(angle(seq(90, 360 + 90, by = 60), "degrees"),
                 radius = c(rep(0.488, 6), 0))
xy$translate(x = 0.5, y = 0.5)
l_xy <- list()
l_xy$top <- xy[c(1, 2, 7, 6)]
l_xy$right <- xy[c(7, 4, 5, 6)]
l_xy$left <- xy[c(2, 3, 4, 7)]

gp_border <- gpar(fill = NA, col = "black", lwd = 12)
vp_define <- viewport(width = unit(3, "inches"), height = unit(3, "inches"))

colors <- c("#D55E00", "#56B4E9", "#009E73")
spacings <- c(0.25, 0.25, 0.2)
texts <- c("pkgname", "right\nface", "left\nface")
rots <- c(45, 0, 0)
fontsizes <- c(52, 80, 80)
sides <- c("top", "right", "left")
types <- gridpattern::names_polygon_tiling[c(5, 9, 7)]
l_grobs <- list()
grid.newpage()
for (i in 1:3) {
    side <- sides[i]
    xy_side <- l_xy[[side]]
    if (requireNamespace("gridpattern", quietly = TRUE)) {
        bg <- gridpattern::grid.pattern_polygon_tiling(
                   colour = "grey80",
                   fill = c(colors[i], "white"),
                   type = types[i],
                   spacing = spacings[i],
                   draw = FALSE)
    } else {
        bg <- rectGrob(gp = gpar(col = NA, fill = colors[i]))
    }
    text <- textGrob(texts[i], rot = rots[i],
                     gp = gpar(fontsize = fontsizes[i]))
    settings <- affine_settings(xy_side, unit = "snpc")
    grob <- l_grobs[[side]] <- grobTree(bg, text)
    grid.affine(grob,
                vp_define = vp_define,
                transform = settings$transform,
                vp_use = settings$vp)
    grid.polygon(xy_side$x, xy_side$y, gp = gp_border)
}

## ----die-faces, fig.width = 4.0, fig.height = 3.0, fig.alt = "The six die faces"----
library("affiner")
library("grid")
xyz_face <- as_coord3d(x = c(0, 0, 1, 1) - 0.5, y = c(1, 0, 0, 1) - 0.5, z = 0.5)
l_faces <- list() # order faces for our target projections
l_faces$bottom <- xyz_face$clone()$
                    rotate("z-axis", angle(180, "degrees"))$
                    rotate("y-axis", angle(180, "degrees"))
l_faces$north <- xyz_face$clone()$
                    rotate("z-axis", angle(90, "degrees"))$
                    rotate("x-axis", angle(-90, "degrees"))
l_faces$east <- xyz_face$clone()$
                    rotate("z-axis", angle(90, "degrees"))$
                    rotate("y-axis", angle(90, "degrees"))
l_faces$west <- xyz_face$clone()$
                    rotate("y-axis", angle(-90, "degrees"))
l_faces$south <- xyz_face$clone()$
                    rotate("z-axis", angle(180, "degrees"))$
                    rotate("x-axis", angle(90, "degrees"))
l_faces$top <- xyz_face$clone()$
                    rotate("z-axis", angle(-90, "degrees"))

colors <- c("#D55E00", "#009E73", "#56B4E9", "#E69F00", "#CC79A7", "#0072B2")
spacings <- c(0.25, 0.2, 0.25, 0.25, 0.25, 0.25)
die_face_grob <- function(digit) {
    if (requireNamespace("gridpattern", quietly = TRUE)) {
        bg <- gridpattern::grid.pattern_polygon_tiling(
                   colour = "grey80",
                   fill = c(colors[digit], "white"),
                   type = gridpattern::names_polygon_tiling[digit],
                   spacing = spacings[digit],
                   draw = FALSE)
    } else {
        bg <- rectGrob(gp = gpar(col = NA, fill = colors[digit]))
    }
    digit <- textGrob(digit, gp = gpar(fontsize = 72))
    grobTree(bg, digit)
}
l_face_grobs <- lapply(1:6, function(i) die_face_grob(i))
grid.newpage()
for (i in 1:6) {
    vp <- viewport(x = unit((i - 1) %% 3 + 1, "inches"),
                   y = unit(3 - ((i - 1) %/% 3 + 1), "inches"),
                   width = unit(1, "inches"), height = unit(1, "inches"))
    pushViewport(vp)
    grid.draw(l_face_grobs[[i]])
    popViewport()
    grid.text("The six die faces", y = 0.9, 
              gp = gpar(fontsize = 18, face = "bold"))
}

## ----projected-die, fig.width = 3.0, fig.height = 3.0, fig.alt = "Parallel projection of a die"----
# re-order face grobs for our target projections
# bottom = 6, north = 4, east = 5, west = 2, south = 3, top = 1
l_face_grobs <- l_face_grobs[c(6, 4, 5, 2, 3, 1)]
draw_die <- function(l_xy, l_face_grobs) {
    min_x <- min(vapply(l_xy, function(x) min(x$x), numeric(1)))
    min_y <- min(vapply(l_xy, function(x) min(x$y), numeric(1)))
    l_xy <- lapply(l_xy, function(xy) {
        xy$translate(x = -min_x + 0.5, y = -min_y + 0.5)
    })
    grid.newpage()
    vp_define <- viewport(width = unit(1, "inches"), height = unit(1, "inches"))
    gp_border <- gpar(col = "black", lwd = 4, fill = NA)
    for (i in 1:6) {
        xy <- l_xy[[i]]
        settings <- affine_settings(xy, unit = "inches")
        grid.affine(l_face_grobs[[i]],
                    vp_define = vp_define,
                    transform = settings$transform,
                    vp_use = settings$vp)
        grid.polygon(xy$x, xy$y, default.units = "inches", gp = gp_border)
    }
}
# oblique projection of dice onto xy-plane
l_xy_oblique1 <- lapply(l_faces, function(xyz) {
    xyz$clone() |>
        as_coord2d(scale = 0.5)
})
draw_die(l_xy_oblique1, l_face_grobs)
grid.text("Oblique projection\n(onto xy-plane)", y = 0.9,
          gp = gpar(fontsize = 18, face = "bold"))

# oblique projection of dice on xz-plane
l_xy_oblique2 <- lapply(l_faces, function(xyz) {
    xyz$clone()$
        permute("xzy") |>
        as_coord2d(scale = 0.5, alpha = angle(135, "degrees"))
})
draw_die(l_xy_oblique2, l_face_grobs)
grid.text("Oblique projection\n(onto xz-plane)", y = 0.9,
          gp = gpar(fontsize = 18, face = "bold"))

# isometric projection
l_xy_isometric <- lapply(l_faces, function(xyz) {
    xyz$clone()$
        rotate("z-axis", angle(45, "degrees"))$
        rotate("x-axis", angle(-(90 - 35.264), "degrees")) |>
        as_coord2d()
})

draw_die(l_xy_isometric, l_face_grobs)
grid.text("Isometric projection", y = 0.9,
          gp = gpar(fontsize = 18, face = "bold"))

