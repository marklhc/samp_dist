# ----------------------------------------------------------------------- #
# 2015 Mark Lai
# 
# Simple app to compare the sampling distribution of the mean and the median
#
# How to use:
#   1. Run the R script in RStudio
#   2. Click on the gear on top left of the `Plots` pane
#   3. Manipulate the population and the sample size to see the respective
#      sampling distributions
# ----------------------------------------------------------------------- #

if (!"manipulate" %in% installed.packages()) {
  install.packages("manipulate", dep = TRUE)
}
PlotDist <- function(x, type = c("mean", "median", "population"), 
                     mu = 10, sd = 5, ...) {
  type = match.arg(type)
  if (type == "mean") xlab = expression(bar(italic(x)))
  else if (type == "median") xlab = expression(italic(Mdn[x]))
  else if (type == "population") xlab = expression(italic(x))
  h1 <- hist(x,  
             xlab = xlab, 
             main = paste("Sampling Distribution of", type), 
             xlim = c(mu - 2 * sd, mu + 2 * sd), freq = FALSE, 
             ...)
  text(mu + sd, max(h1$density) * .95, 
       bquote(mean == .(round(mean(x), 2))), 
       pos = 4) 
  text(mu + sd, max(h1$density) * .80, 
       bquote(median == .(round(median(x), 2))), 
       pos = 4) 
  text(mu + sd, max(h1$density) * .65, 
       bquote(italic(SD) == .(round(sd(x), 2))), 
       pos = 4)
}

library(manipulate)
manipulate(
  {
    if (dist == "normal") {
      sam <- matrix(rnorm(x * 2e4, mean = 10, sd = 5), nrow = x)
    } else if (dist == "Student's t") {
      sam <- matrix(rt(x * 2e4, df = df) * 5 / sqrt(df / (df - 1)) + 10, 
                    nrow = x)
    }
    xbars <- colMeans(sam)
    mdnx <- apply(sam, 2, median)
    
    layout(matrix(c(1, 1, 2, 3), 2, 2))
    PlotDist(sam, sd = 10, type = "pop", 
             breaks = c(-Inf, seq(-10, 30, length.out = 30), Inf))
    PlotDist(xbars, col = "grey", 
             breaks = c(-Inf, seq(10 - 20 / sqrt(x), 10 + 20 / sqrt(x), 
                                  length.out = 15), Inf))
    PlotDist(mdnx, type = "median", col = "blue", 
             breaks = c(-Inf, seq(10 - 20 / sqrt(x), 10 + 20 / sqrt(x), 
                                  length.out = 20), Inf))
    par(mfrow = c(1, 1))
  },
  x = slider(2, 50, label = "Sample Size (n)"), 
  dist = picker("normal", "Student's t", label = "Population Distribution"),
  df = slider(3, 30, label = "df for t-distribution"))