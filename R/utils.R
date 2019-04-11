#' @keywords internal
make_field <- function(x) {
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.null(x) || length(x) != 1L || is.na(x)) '' else as.character(x)
}

#' @keywords internal
counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
  }
}
