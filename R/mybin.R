#' My binomial distribution samples
#'
#' @param iter Number of plots
#' @param n Sample size
#' @param p Probability
#'
#' @returns
#' @export
#'
#' @examples
#' mybin()
mybin <- function(
    iter = 100, # iterations
    n = 10, # sample size
    p = 0.5
) {
  # Samples matrix (initially all NA)
  sam.mat <- matrix(
    NA,
    nr = n,
    nc = iter,
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
