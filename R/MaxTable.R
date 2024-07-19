#' Calculate the Maximum Value(s) and Corresponding Level(s) of a Factor Variable
#'
#' This function returns the level(s) corresponding to the maximum value(s) of a factor variable.
#'
#' @param InVec Input vector, expected to be a factor variable or convertible to a factor.
#' @param mult Logical value indicating whether to return multiple maximum values or just the first one. Default is FALSE.
#' @return If \code{mult} is FALSE, returns the level corresponding to the maximum value of the factor variable.
#'         If \code{mult} is TRUE, returns a character vector containing all the levels with the maximum value.
#' @examples
#' vec <- factor(c("A", "B", "A", "C", "B", "B"))
#' MaxTable(vec) # Returns "A"
#' MaxTable(vec, mult = TRUE) # Returns c("A", "B")
#' @export

MaxTable <- function(InVec, mult = FALSE) {
  if (length(InVec) == 0) {
    return(character(0))
  }
  if (!is.factor(InVec)) {
    InVec <- factor(InVec)
  }
  A <- tabulate(InVec)
  if (isTRUE(mult)) {
    levels(InVec)[A == max(A)]
  } else {
    levels(InVec)[which.max(A)]
  }
}
