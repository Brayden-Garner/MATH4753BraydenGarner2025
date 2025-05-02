#' Bootstrap Confidence Interval Visualization
#'
#' This function performs non-parametric bootstrapping to estimate the
#' distribution of a statistic and plots a histogram
#' of the bootstrapped values. It also overlays the point estimate and
#' confidence interval.
#'
#' @param iter Integer. Number of bootstrap resampling iterations. Default is 10,000.
#' @param x Numeric vector. The original sample data to bootstrap from.
#' @param fun Function or function name as a string. The statistic to compute on each resample. Default is `"mean"`.
#' @param alpha Numeric. Significance level for the confidence interval (e.g., 0.05 for a 95% CI). Default is 0.05.
#' @param cx Numeric. Text scaling factor for annotation labels on the plot. Default is 1.5.
#' @param ... Additional graphical parameters passed to `hist()`.
#'
#' @importFrom stats quantile
#' @importFrom graphics hist abline segments text
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{ci}{Numeric vector. Lower and upper bounds of the confidence interval.}
#'   \item{fun}{The statistic function used.}
#'   \item{x}{The original input data.}
#' }
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(30, mean = 5, sd = 2)
#' myboot2(iter = 1000, x = data, fun = "mean", alpha = 0.05)
#'
#' @export
myboot2 <- function(iter = 10000,
                    x,
                    fun = "mean",
                    alpha = 0.05,
                    cx = 1.5,
                    ...) {
  n <- length(x)

  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun) # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para = hist(
    xstat,
    freq = FALSE,
    las = 1,
    main = paste(
      "Histogram of Bootstrap sample statistics",
      "\n",
      "alpha=",
      alpha,
      " iter=",
      iter,
      sep = ""
    ),
    ...
  )

  # mat will be a matrix that contains the data, this is done so that I can use apply()
  mat = matrix(x,
               nrow = length(x),
               ncol = 1,
               byrow = TRUE)

  # pte is the point estimate
  # This uses whatever fun is
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")# Vertical line
  segments(ci[1], 0, ci[2], 0, lwd = 4)      #Make the segment for the ci
  text(ci[1],
       0,
       paste("(", round(ci[1], 2), sep = ""),
       col = "Red",
       cex = cx)
  text(ci[2],
       0,
       paste(round(ci[2], 2), ")", sep = ""),
       col = "Red",
       cex = cx)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x))# Some output to use if necessary
}
