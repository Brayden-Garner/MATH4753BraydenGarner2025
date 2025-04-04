#' My n tickets function
#'
#' @param N Number of seats
#' @param gamma Acceptable overbooking probability
#' @param p Probability that an individual shows up
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist lines par polygon text
#' @importFrom stats density dnorm dunif pbinom pnorm runif
#'
#' @returns list: nd (discrete), nc (continuous), N, gamma, and p
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p) {

  # Discrete Calculation
  objective_discrete <- function(n) {
    return(1 - gamma - pbinom(N, n, p))
  }

  # Normal Approximation
  objective_normal <- function(n) {
    return(1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p))))
  }

  # Searching for optimal n for both cases; n must be an integer
  n_values <- seq(N, N*2, by = 1)

  # Find n for discrete using absolute min of the objective function
  objective_discrete_values <- sapply(n_values, objective_discrete)
  nd_index <- which.min(abs(objective_discrete_values))
  n_discrete <- n_values[nd_index]

  # Find n for normal approximation using absolute min of the objective function
  objective_normal_values <- sapply(n_values, objective_normal)
  nc_index <- which.min(abs(objective_normal_values))
  n_normal <- n_values[nc_index]

  # Create a named list of results
  result <- list(nd = n_discrete, nc = n_normal, N = N, p = p, gamma = gamma)
  nd = n_discrete; nc = n_normal

  # Print the result
  print(result)

  # Plot the objective functions vs. n

  # Set up side by side plot without ggplot because its too much work
  par(mfrow = c(1, 2))

  # Plot the first graph objective_discrete_values
  plot(
    n_values,
    objective_discrete_values,
    type = "o",
    col = "blue",
    pch = 16,
    lty = 1,
    cex = 0.7,
    xlab = "n (Tickets Sold)",
    ylab = "Objective Function",
    main = paste("Objective Vs n to find optimal tickets sold\n(", nd,") gamma=", gamma, " N=", N, "discrete"),
    xlim = c(N, N*1.1),
    col.lab = "black",
    col.axis = "black"
  )

  # Add a vertical line
  abline(v = nd, col = "red", lty = 1)  # Vertical line

  # y-value where the curve intersects the vertical line
  y_at <- objective_discrete_values[which.min(abs(n_values - nd))]

  # horizontal line
  abline(h = y_at, col = "red", lty = 1)  # Horizontal line

  # Plot the second graph objective_normal_values
  plot(
    n_values,
    objective_normal_values,
    type = "l",
    col = "black",
    xlab = "n (Tickets Sold)",
    ylab = "Objective",
    main = paste("Objective Vs n to find optimal tickets sold\n(", nc,") gamma=", gamma, " N=", N, "continuous"),
    xlim = c(N, N*1.1),
    col.lab = "black",
    col.axis = "black"
  )

  # Add a vertical line at x = 204
  abline(v = nc, col = "blue", lty = 1)  # Vertical line

  y_at <- objective_normal_values[which.min(abs(n_values - nc))]

  # horizontal line
  abline(h = y_at, col = "blue", lty = 1)
}
