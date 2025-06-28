#' Display the package name in colorful ASCII art
#'
#' Prints an ASCII art logo of the package name ``tyler`` with each
#' letter shown in a different color. The function uses ANSI escape
#' codes so it works in terminals that support colored output.
#'
#' @return No return value. Called for its side effect of printing the
#'   ASCII art logo to the console.
#' @examples
#' tyler_ascii_art()
#' @export
tyler_ascii_art <- function() {
  letters_art <- list(
    T = c(" _____ ", "|_   _|", "  | |  ", "  | |  ", "  |_|  "),
    Y = c("__   __", "\\ \\ / /", " \\ V / ", "  | |  ", "  |_|  "),
    L = c(" _     ", "| |    ", "| |    ", "| |___ ", "|_____|"),
    E = c(" _____ ", "| ____|", "|  _|  ", "| |___ ", "|_____|"),
    R = c(" ____  ", "|  _ \\", "| |_) |", "|  _ < ", "|_| \\_\\")
  )
  colors <- c("\033[31m", "\033[32m", "\033[33m", "\033[34m", "\033[35m")
  for (i in seq_len(5)) {
    line <- paste(
      paste0(colors[1], letters_art$T[i], "\033[0m"),
      paste0(colors[2], letters_art$Y[i], "\033[0m"),
      paste0(colors[3], letters_art$L[i], "\033[0m"),
      paste0(colors[4], letters_art$E[i], "\033[0m"),
      paste0(colors[5], letters_art$R[i], "\033[0m"),
      sep = " "
    )
    cat(line, "\n")
  }
}
