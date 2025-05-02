#' Myncurve: Plot Normal Distribution and Compute Area Under the Curve
#'
#' This function plots the normal distribution curve for a given mean (`mu`) and standard deviation (`sigma`),
#' and shades the area under the curve from negative infinity up to a specified value `a`. It also returns
#' the mean, standard deviation, and the computed probability (area) from negative infinity to `a`.
#'
#' @param mu A numeric value representing the mean of the normal distribution (default is 0).
#' @param sigma A numeric value representing the standard deviation of the normal distribution (default is 1).
#' @param a A numeric value indicating the point up to which the area under the normal curve will be computed and shaded.
#' @param x A numeric vector for the x-values over which the normal distribution curve will be plotted.
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist lines par polygon text
#' @importFrom stats density dnorm dunif pbinom pnorm runif
#'
#' @returns A list containing:
#' \item{mu}{The mean of the normal distribution (input value).}
#' \item{sigma}{The standard deviation of the normal distribution (input value).}
#' \item{area}{The area (probability) under the normal curve from -∞ to the specified point `a` (computed using `pnorm`).}
#'
#' @details This function allows users to visualize the normal distribution and compute the cumulative probability
#' for any given value of `a`. The area under the curve represents the cumulative probability of obtaining a value
#' less than or equal to `a` for a normal distribution with the specified mean and standard deviation.
#'
#' @examples
#' # Plot normal distribution with mu = 0, sigma = 1, and compute the area up to a = 1.5
#' mycurve(mu = 0, sigma = 1, a = 1.5, x = seq(-3, 3, by = 0.1))
#'
#' # Plot normal distribution with mu = 2, sigma = 1, and compute the area up to a = 2.5
#' mycurve(mu = 2, sigma = 1, a = 2.5, x = seq(-1, 5, by = 0.1))
#'
#' @export
myncurve <- function(mu = 0, sigma = 1, a, x) {
  curve(
    dnorm(x, mean = mu, sd = sigma),
    xlim = c(mu - 3 * sigma, mu + 3 * sigma)
  )
  # Curve points coordinates
  xcurve <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)

  # Points in inequality
  xfill <- xcurve[xcurve <= a]
  yfill <- dnorm(xfill, mu, sigma)

  # Draw new plot
  curve(
    dnorm(x, mean = mu, sd = sigma),
    xlim = c(mu - 3 * sigma, mu + 3 * sigma)
  )

  # Fill polygon from vertices
  if (mu - 3 * sigma < a) {
    polygon(
      c(mu - 3 * sigma, xfill, a),
      c(0, yfill, 0),
      col = "black"
    )
  }

  # Area (Probability) for label
  prob <- round( # to right bound - to left bound
    pnorm(a, mu, sigma),
    4 # digits
  )

  # Area label
  text(
    (a - mu - 3 * sigma) / 2,
    pnorm((a - mu - 3 * sigma) / 2, 3, 5) / 2,
    paste("Area = ", prob, sep = "")
  )

  area <- pnorm(a, mu, sigma)
  list(mu = mu, sigma = sigma, area = area)
}
