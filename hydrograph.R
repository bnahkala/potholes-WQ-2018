# Brady Nahkala
# 9.25.2020

# Script modified from hydrograph() function in 
# EcoHydRology package hosted on rForge

hydrograph2 <-
  function (input = matrix(ncol = 2, nrow = 2),
            streamflow = input[,
                               2],
            timeSeries = input[, 1],
            streamflow2 = NULL,
            precip = NULL,
            begin = 1,
            endindex = length(streamflow),
            P.units = "",
            S.units = "",
            S1.col = "black",
            S2.col = "red",
            stream.label = "Water Depth (m)",
            streamflow3 = NULL,
            streamflow4 = NULL,
            streamflow5 = NULL,
            streamflow6 = NULL,
            streamflow7 = NULL,
            streamflow8 = NULL,
            streamflow9 = NULL,
            streamflow10 = NULL,
            precip2 = NULL) {
    
    # PRECIP      
    if (is.null(streamflow2) & (ncol(input) > 3))
      streamflow2 <- input[, 4]
    if (is.null(precip) & (ncol(input) > 2)) {
      precip <- input[, 2]
      streamflow <- input[, 3]
    }
    if (!is.null(precip))  {
      par(mar = c(3, 5, 1, 4))
      barplot(
        precip[begin:endindex],
        yaxt = "n",
        space = NULL,
        ylim = rev(c(0, 4 * max(na.omit(
          precip[begin:endindex]
        )))),
        xaxt = "n"
      )
      
      axis(
        side = 3,
        pos = 0,
        tck = 0,
        xaxt = "n"
      )
      r <-
        max(ceiling(-log10(max(na.omit(
          precip[begin:endindex]
        )))), 0)
      axis(
        side = 4,
        at = seq(0, round(max(na.omit(
          precip[begin:endindex]
        )) +
          10 ^ (-r), r), length = (1 + ifelse(
            round(max(na.omit(precip[begin:endindex])) +
                    10 ^
                    (-r), r) < 10 * 10 ^ (-r), floor(max(na.omit(precip[begin:endindex])) *
                                                       10 ^ r +
                                                       1), 4
          ))),
        labels = seq(
          0,
          round(max(na.omit(precip[begin:endindex])) +
                  10 ^
                  (-r), r),
          length = (1 + ifelse(round(max(
            na.omit(precip[begin:endindex])
          ) +
            10^(-r), r) < 10*10^(-r), floor(max(na.omit(precip[begin:endindex]))*10^r + 1), 
          4))))
      if (P.units=="") {
        mtext(paste("Precipitation", P.units), 4, line = 2, cex = 0.9, adj = 1)
      } else  mtext(paste("Precipitation (", P.units, ")", sep=""), 4, line = 2, cex = 0.9, adj = 1)
      par(new = T)
    }
    if (!is.null(precip2)){
      barplot(precip2[begin:endindex], yaxt = "n", space = NULL, col="pink",
              ylim = rev(c(0, 4 * max(na.omit(precip[begin:endindex])))), 
              xaxt = "n")
      par(new = T)
    }
    
    mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(11)
    labs <- c("Bunny", "Cardinal", "Gravy", "Hen", "Lettuce", "Mouth", "Plume", "Turkey", "Walnut","Yam")
    
    
    # PLOT STREAMFLOW    
    plot(streamflow[begin:endindex], col = mycolors[1], type = "l", 
         lwd = 2, ylab = stream.label, xaxt = "n", xlab = "date", 
         ylim = c(0, 1.2 * max(na.omit(streamflow[begin:endindex]), na.omit(streamflow2[begin:endindex]))), 
         axes = FALSE)
    
    
    #mtext (expression(paste("                              ", " (" , m^3/s, ")", sep="")), 2,3)
    if (S.units=="m3/s" | S.units=="m3s"){
      mtext (expression(paste(" (" , m^3/s, ")", sep="")), 2,1.5)
    } else if (S.units=="ft3/s" | S.units=="ft3s") {
      mtext (expression(paste(" (" , ft^3/s, ")", sep="")), 2,1.5)
    } else if (S.units!="") mtext (paste(" (" , S.units, ")", sep=""), 2,1.5)
    lines(streamflow2[begin:endindex], col = mycolors[2], lwd = 2, 
          lty = 1, xaxt = "n")
    if (!is.null(streamflow3)){
      lines(streamflow3[begin:endindex], col = mycolors[3], lwd = 2,   ##potential for more streamflows
            lty = 1, xaxt = "n")
    }
    if (!is.null(streamflow4)){
      lines(streamflow4[begin:endindex], col = mycolors[4], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    
    # BRADY ADDED EXTRA FLOWS
    if (!is.null(streamflow5)){
      lines(streamflow5[begin:endindex], col = mycolors[5], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    if (!is.null(streamflow6)){
      lines(streamflow6[begin:endindex], col = mycolors[6], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    if (!is.null(streamflow7)){
      lines(streamflow7[begin:endindex], col = mycolors[7], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    if (!is.null(streamflow8)){
      lines(streamflow8[begin:endindex], col = mycolors[8], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    if (!is.null(streamflow9)){
      lines(streamflow9[begin:endindex], col = mycolors[9], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    if (!is.null(streamflow10)){
      lines(streamflow10[begin:endindex], col = mycolors[10], lwd = 2,   ##potential for more streamflows
            lty=1, xaxt = "n")
    }
    axis(side = 1, at = seq(1, (endindex - begin + 1), length = 14), 
         pos = 0, labels = format(timeSeries[seq(begin, endindex, 
                                                 length = 14)], "%d-%b-%y"))
    legend("bottomright", inset = c(0.05, 0.475),legend=c(labs), col=c(mycolors), lty=1, cex=0.5)
    
    
    axis(side = 2, pos = 0)
  }

