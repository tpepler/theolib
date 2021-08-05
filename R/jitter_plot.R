## Produce grouped plot of jittered data values. Used as alternative to grouped box plot.

jitter_plot <- function(x, # vector containing quantitative data values
                        groups, # vector containing grouping variable
                        ylim = c(0.9 * min(x, na.rm = T), 1.1 * max(x, na.rm = T)), # limits of vertical axis
                        main = '', # plot title
                        xlab = '', # horizontal axis title
                        ylab = '', # vertical axis title
                        col = 'black', # colour of points
                        pch = 1, # plotting character
                        cex.xaxis = 0.8, # character expansion factor for group labels on x-axis
                        plotmeans = TRUE, # logical: should the group means be shown?
                        plot2se = TRUE){ # logical: should the (2 *) standard error of the group means be shown?

  # set of group names
  grpnames <- sort(unique(groups))
  # number of groups
  ngroups <- length(grpnames)

  # save current graphical parameter settings
  #oldpar <- par('ann')
  # suppress printing of axes labels
  #par(ann = FALSE)

  # produce empty plot
  plot(x = c(0.5, ngroups + 0.5),
       y = c(ylim[1], ylim[2]),
       type = 'n',
       main = main,
       xlab = xlab,
       ylab = ylab,
       xaxt = 'n',
       yaxt = 'n',
       frame.plot = TRUE)
  # add jittered points per group
  for(i in 1:ngroups){
    plotdata <- x[groups == grpnames[i]]
    points(x = jitter(rep(i, times = length(plotdata)), amount = 0.2),
           y = plotdata,
           pch = pch,
           col = col)
    # plot 2 * standard error bars (optional)
    if(plot2se){
      sem <- sd(plotdata, na.rm = T) / sqrt(sum(!is.na(plotdata)))
      segments(x0 = i,
               x1 = i,
               y0 = mean(plotdata, na.rm = T) - 2 * sem,
               y1 = mean(plotdata, na.rm = T) + 2 * sem,
               col = 'black', lwd = 2)
    }
    # indicate group means (optional)
    if(plotmeans){
      segments(x0 = i - 0.25,
               x1 = i + 0.25,
               y0 = mean(plotdata, na.rm = T),
               y1 = mean(plotdata, na.rm = T),
               col = 'red', lwd = 2)
    }
  }

  # add y-axis
  axis(side = 2)
  # add x-axis
  axis(side = 1, at = c(1:ngroups), labels = grpnames, cex.axis = cex.xaxis)
}
