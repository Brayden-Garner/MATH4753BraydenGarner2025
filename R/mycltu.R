#' My Central Limit Theorem Uniform Function
#'
#' This function simulates the Central Limit Theorem (CLT) by taking repeated samples from a uniform distribution
#' and computing their means. It then visualizes the distribution of these sample means, comparing it with both
#' a density plot of the sample means and a theoretical normal distribution.
#'
#' @param n    An integer specifying the sample size for each iteration.
#'             Represents the number of random samples drawn from the uniform distribution in each iteration.
#' @param iter An integer specifying the number of iterations (samples).
#'             This determines how many sample means will be computed.
#' @param a    A numeric value specifying the lower bound of the uniform distribution. Default is 0.
#' @param b    A numeric value specifying the upper bound of the uniform distribution. Default is 10.
#' @param x    A numeric vector representing the range of values over which to plot the theoretical normal and uniform distributions.
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist lines par polygon text
#' @importFrom stats density dnorm dunif pbinom pnorm runif
#'
#' @returns This function does not return any value. It generates a histogram of the sample means with an overlaid
#' theoretical normal distribution curve and the density curve of the sample means.
#'
#' @export
#'
#' @examples
#' # Simulate the CLT with sample size 30, 1000 iterations, and uniform distribution between 0 and 10
#' mycltu(n = 30, iter = 1000, a = 0, b = 10, x = seq(-5, 15, by = 0.1))
mycltu <- function(n, iter, a = 0, b = 10, x) {
  ## r-random sample from the uniform
  y = runif(n * iter, a, b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w = apply(data, 2, mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param = hist(w, plot = FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax = max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax = 1.1 * ymax
  ## Now we can make the histogram
  hist(
    w,
    freq = FALSE,
    ylim = c(0, ymax),
    main = paste("Histogram of sample mean", "\n", "sample size= ", n, sep =
                   ""),
    xlab = "Sample mean"
  )
  ## add a density curve made from the sample distribution
  lines(density(w), col = "Blue", lwd = 3) # add a density plot
  ## Add a theoretical normal curve
  curve(
    dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))),
    add = TRUE,
    col = "Red",
    lty = 2,
    lwd = 3
  ) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x, a, b), add = TRUE, lwd = 4)
}
