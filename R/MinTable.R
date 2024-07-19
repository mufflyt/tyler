#' Calculate the Minimum Value(s) and Corresponding Level(s) of a Factor Variable
#'
#' This function returns the level(s) corresponding to the minimum value(s) of a factor variable.
#'
#' @param InVec Input vector, expected to be a factor variable or convertible to a factor.
#' @param mult Logical value indicating whether to return multiple minimum values or just the first one. Default is FALSE.
#' @return If \code{mult} is FALSE, returns the level corresponding to the minimum value of the factor variable.
#'         If \code{mult} is TRUE, returns a character vector containing all the levels with the minimum value.
#' @examples
#' vec <- factor(c("A", "B", "A", "C", "B", "B"))
#' MinTable(vec) # Returns "C"
#' MinTable(vec, mult = TRUE) # Returns "C"
#' @export

MinTable <- function(InVec, mult = FALSE) {
  if (!is.factor(InVec) || length(InVec) == 0) {
    return(character(0))
  }
  InVec <- factor(InVec)
  A <- tabulate(InVec)
  if (isTRUE(mult)) {
    levels(InVec)[A == min(A)]
  } else {
    levels(InVec)[which.min(A)]
  }
}

