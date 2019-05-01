#' @keywords internal
TagValue <- function(tag = NA_character_,
                     value = NA_character_){
  structure(
    list(tag = tag,
         value = value
         )
  )
}

#' @keywords internal
DeltaNeutralContract <- function(conId = 0L,
                                 delta = 0.0,
                                 price= 0.0) {
  structure(
    list(
      conId = conId,
      delta = delta,
      price= price
    ),
    class = "DeltaNeutralContract"
  )
}
