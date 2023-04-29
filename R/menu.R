.onAttach <- function(...) {
  header <- cli::rule(
    left = cli::style_bold("Discrete Choice Modeling in R"),
    right = paste0("DCM ", "0.1.2")
  )
  inform_startup(paste0(header, "\n"), paste0(c("Can", "also", "print", "more"), sep="\n"))

}

inform_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }

  rlang::inform(msg, ..., class = "packageStartupMessage")
}
