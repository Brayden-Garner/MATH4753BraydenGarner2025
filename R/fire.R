#' Fire Damage Data Set
#'
#' This data set contains information about fire incidents, including the distance from a specific location
#' and the resulting damage caused by the fire. It can be used for analysis and modeling related to fire incidents
#' and their impact.
#'
#' @format A data frame with 15 observations and 2 variables:
#' \describe{
#'   \item{DISTANCE}{Numeric. The distance from a specific location (in miles).}
#'   \item{DAMAGE}{Numeric. The damage caused by the fire, quantified as a monetary value in thousands of United States dollars.}
#' }
#' @source Unknown. Data provided by an unspecified source.
#' @examples
#' # Accessing the data set
#' head(fire)  # Display first few rows of the fire data set
#'
#' # Simple analysis example
#' summary(fire$DISTANCE)  # Summary of the distance values
#' plot(fire$DISTANCE, fire$DAMAGE, main = "Fire Damage vs Distance", xlab = "Distance", ylab = "Damage")
"fire"
