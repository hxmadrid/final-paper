

#  converts values from timer to seconds

#### TIMER CONVERT FUNCTION START ####
#### TIMER CONVERT FUNCTION START ####

rb.time <- function(time.val) {
  
  
  decimal <- strsplit(time.val, "\\.")[[1]]       # separates values by decimal point; error w/o [[1]] no idea why
  if (length(decimal) == 2 && nchar(decimal[2]) == 1) {  # makes trailing 0s significant
    decimal[2] <- paste0(decimal[2], "0")                # so 1.30 = 90 sec and 1.3 = 63 sec
  }
  
  min <- as.numeric(decimal[1])     # assigns decimal parts for formula
  sec <- as.numeric(decimal[2])     # left side 'min' right side 'sec' so 'min.sec'
  sec.val <- (min * 60) + sec       # main formula
  return(sec.val)
  
  
}

#### TIMER CONVERT FUNCTION END ####
#### TIMER CONVERT FUNCTION END ####


#  Games-Howell post-hoc for Welch's ANOVA

#### 
plotGamesHowell <- plotGamesHowell <- function(ght.out,
                                               x.axis.label = "Comparison",
                                               y.axis.label = "Effect Size",
                                               axis.adjust = 0,
                                               adjust.x.spacing = 5){
  
  #ght.out <- as.data.frame(ght.out[[1]])
  means <- ght.out$intermediate$dmeans
  categories <- ght.out$intermediate$pairNames
  groups <- length(categories)
  ci.low <- ght.out$intermediate$gh.low
  ci.up  <- ght.out$intermediate$gh.high                         
  
  n.means <- length(means)
  
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
  
  # calculate values for plotting limits            
  y.max <- max(ci.up) +                    
    max(ci.up)*axis.adjust
  y.min <- min(ci.low) - 
    max(ci.low)*axis.adjust
  
  if(groups == 2){ x.values <- c(0.25, 0.5)}
  if(groups == 3){ x.values <- c(0.25, 0.5,0.75)}
  
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  axis(side = 1, 
       at = x.values,
       labels = categories,
  )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.up,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.low,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  #add reference line at 0
  abline(h = 0, col = 2, lwd = 2, lty =2)
  
  mtext(text = x.axis.label,side = 1,line = 1.75)
  mtext(text = y.axis.label,side = 2,line = 1.95)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  
}

#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
