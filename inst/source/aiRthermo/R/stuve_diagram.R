stuve_diagram <-
function (Pres, Temp, TempD = NA, XLIM = c(-80, 45), YLIM = c(1050, 
    100), col.lines = NULL, lty.lines = NULL, lwd.lines = NULL) 
{
    lines = export_lines()
    if (length(col.lines) == 6) {
        names(col.lines) <- c("isotherms", "isobars", "theta", 
            "adiabat", "wsat", "sound")
    }
    else {
        col.lines <- c("grey", "grey", "olivedrab ", "olivedrab ", 
            "brown", "red")
        names(col.lines) <- c("isotherms", "isobars", "theta", 
            "adiabat", "wsat", "sound")
    }
    if (length(lty.lines) == 6) {
        names(lty.lines) <- c("isotherms", "isobars", "theta", 
            "adiabat", "wsat", "sound")
    }
    else {
        lty.lines <- c(3, 3, 3, 1, 3, 1)
        names(lty.lines) <- c("isotherms", "isobars", "theta", 
            "adiabat", "wsat", "sound")
    }
    if (length(lwd.lines) == 6) {
        names(lwd.lines) <- c("isotherms", "isobars", "theta", 
            "adiabat", "wsat", "sound")
    }
    else {
        lwd.lines <- c(2, 2, 2, 1, 2, 1)
        names(lwd.lines) <- c("isotherms", "isobars", "theta", 
            "adiabat", "wsat", "sound")
    }
    if (min(YLIM) >= 20) {
        deglab <- expression(""^o * plain("C"))
        deglab2 <- expression("         "^o * plain("C"))
        plot(lines[["adiabat_x_T"]][1, ], lines[["adiabat_y_T"]][1, 
            ], type = "l", xlim = XLIM, ylim = YLIM, log = "y", 
            col = col.lines["adiabat"], xlab = deglab, ylab = "hPa", 
            lty = lty.lines["adiabat"], lwd = lwd.lines["adiabat"], 
            axes = FALSE, xaxs = "i", yaxs = "i")
        for (i in 2:dim(lines[["adiabat_x_T"]])[1]) {
            lines(lines[["adiabat_x_T"]][i, ], lines[["adiabat_y_T"]][i, 
                ], col = col.lines["adiabat"], lty = lty.lines["adiabat"], 
                lwd = lwd.lines["adiabat"])
        }
        if (abs(YLIM[2] - YLIM[1]) > 900) {
            chosenlines <- seq(from = 1, to = dim(lines[["theta_x_T"]])[1], 
                by = 2)
            for (i in chosenlines) {
                lines(lines[["theta_x_T"]][i, ], lines[["theta_y_T"]][i, 
                  ], col = col.lines["theta"], lty = lty.lines["theta"], 
                  lwd = lwd.lines["theta"])
            }
        }
        else {
            for (i in 1:dim(lines[["theta_x_T"]])[1]) {
                lines(lines[["theta_x_T"]][i, ], lines[["theta_y_T"]][i, 
                  ], col = col.lines["theta"], lty = lty.lines["theta"], 
                  lwd = lwd.lines["theta"])
            }
        }
        for (i in 1:dim(lines[["wsat_x_T"]])[1]) {
            lines(lines[["wsat_x_T"]][i, ], lines[["wsat_y_T"]][i, 
                ], col = col.lines["wsat"], lty = lty.lines["wsat"], 
                lwd = lwd.lines["wsat"])
        }
        axis(1)
        Levels_y <- c(1000, 925, 850, 700, 500, 400, 300, 250, 
            200, 150, 100, 50, 20)
        NN <- which(Levels_y <= YLIM[1] & Levels_y >= YLIM[2])
        Levels_y <- Levels_y[NN]
        axis(2, at = Levels_y, labels = FALSE)
        text(y = Levels_y, par("usr")[1], labels = Levels_y, 
            srt = 0, pos = 2, xpd = TRUE)
        abline(h = Levels_y, col = col.lines["isobars"], lty = lty.lines["isobars"], 
            lwd = lwd.lines["isobars"])
        grid(col = col.lines["isotherms"], lty = lty.lines["isotherms"], 
            lwd = lwd.lines["isobars"])
        mtext("g/kg     ", line = 1, side = 3, col = col.lines["adiabat"], 
            cex = 1, font = 2)
        N <- which(lines[["wsat_y_T"]][1, ] == YLIM[2])
        NN <- which(lines[["wsat_x_T"]][, N] >= (XLIM[1] + 5) & 
            lines[["wsat_x_T"]][, N] <= (XLIM[2] - 0.01))
        mtext(lines[["wsat_z_T"]][NN], at = lines[["wsat_x_T"]][NN, 
            N], side = 3, col = col.lines["wsat"], lty = lty.lines["wsat"], 
            lwd = lwd.lines["wsat"], cex = 0.75, font = 2, line = -1.75)
        mtext(deglab2, col = col.lines["wsat"], cex = 1, line = 1, 
            font = 2)
        N <- which(lines[["adiabat_y_T"]][1, ] == YLIM[2])
        NN <- which(lines[["adiabat_x_T"]][, N] >= (XLIM[1] + 
            5) & lines[["adiabat_x_T"]][, N] <= (XLIM[2] - 0.01))
        NN <- NN[-length(NN)]
        mtext(lines[["adiabat_z_T"]][NN], at = lines[["adiabat_x_T"]][NN, 
            N] + 0.25, side = 3, adj = 1, col = col.lines["adiabat"], 
            cex = 0.75, font = 2, line = -1)
        lines(Temp, Pres, col = col.lines["sound"], lty = lty.lines["sound"], 
            lwd = lwd.lines["sound"])
        if (is.na(TempD[1]) == FALSE) {
            lines(TempD, Pres, col = col.lines["sound"], lty = lty.lines["sound"], 
                lwd = lwd.lines["sound"])
        }
        box()
        p <- recordPlot()
        return(p)
    }
    else {
        print("XLIM should not be less than 20 hPa")
    }
}
