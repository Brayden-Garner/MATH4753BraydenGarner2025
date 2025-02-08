#' My trend scatter plot function
#'
#' @param fValue a numeric value
#'
#' @returns a graphical plot
#' @export
#'
#' @examples
#' myTrendScatter(0.7)
myTrendScatter <- function(fValue) {
  trendscatter(Height ~ BHDiameter, f = fValue, data = spruce.df, main = paste("f value: ", fValue), xlab = "Breast height diameter (cm)", ylab = "Height (m)")
}
