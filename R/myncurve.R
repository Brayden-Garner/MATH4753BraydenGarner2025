#' Myncurve function
#'
#' @param mu A real number
#' @param sigma A real number
#' @param a A real number
#' @param x a variable
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist lines par polygon text
#' @importFrom stats density dnorm dunif pbinom pnorm runif
#'
#' @returns mu, sigma, and the area under the normal from -inf to a
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
