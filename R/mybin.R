#' Simulate Binomial Distribution Samples
#'
#' This function simulates multiple binomial trials and generates a bar plot
#' showing the proportion of successes across all iterations. The function
#' allows you to control the number of iterations, sample size, and the
#' probability of success for each trial.
#'
#' @param iter An integer. The number of simulations (iterations) to run.
#'              Default is 100.
#' @param n An integer. The sample size (number of trials) for each simulation.
#'          Default is 10.
#' @param p A numeric value between 0 and 1. The probability of success for each trial.
#'          Default is 0.5.
#'
#' @returns A vector of proportions of successes for each iteration.
#'          A bar plot is also displayed showing the distribution of successes.
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist lines par polygon text
#' @importFrom stats density dnorm dunif pbinom pnorm runif
#'
#' @examples
#' # Simulate binomial distribution with default parameters
#' mybin()
#'
#' # Simulate binomial distribution with 200 iterations, 50 trials per iteration, and success probability of 0.7
#' mybin(iter = 200, n = 50, p = 0.7)
#'
#' @export
mybin <- function(
    iter = 100, # iterations
    n = 10, # sample size
    p = 0.5
) {
  # Samples matrix (initially all NA)
  sam.mat <- matrix(
    NA,
    nrow = n,
    ncol = iter,
    byrow = TRUE
  )
  # Trials successes vector
  succ <- c()
  for(i in 1:iter) {
    # Write new samples to columns
    sam.mat[,i] <- sample(
      c(1, 0),
      n,
      replace = TRUE,
      prob = c(p, 1 - p)
    )
    # Sum sample statistic
    succ[i] <- sum(sam.mat[,i])
  }
  # Successes table
  succ.tab <- table(
    factor(succ, levels = 0:n)
  )
  # Proportions bar plot
  barplot(
    succ.tab / iter,
    col = rainbow(n + 1),
    main = "Binomial simulation",
    xlab = "Number of successes"
  )
  succ.tab / iter
}
