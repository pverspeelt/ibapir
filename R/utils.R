#' @importFrom glue glue
#'
#' @keywords internal
make_field <- function(x) {
  if (is.logical(x) && !is.na(x)) {
    return(as.integer(x))
  }
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    ""
    } else as.character(x)
}

#' @keywords internal
counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
  }
}

#' @keywords internal
decoder <- function(what, field){

  if (what == "character") {
    return(field)
  }

  if (what == "bool") {
    if (as.integer(field) == 0){
      field <- FALSE
      return(field)
      } else {
        field <- TRUE
        return(field)
      }
  }

  if (is.null(field) || nchar(field) == 0){
    if (what == "numeric"){
      field <- NA_real_
      return(field)
    } else if (what == "integer"){
      field <- NA_integer_
      return(field)
    } else {
      warning(glue("unsupported desired type for empty value {what}"))
      }
  } else if (what == "numeric") {
    field <- as.numeric(field)
    return(field)
  } else {
    field <- as.integer(field)
    }

  return(field)
}
