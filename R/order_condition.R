#' @keywords internal
PriceCondition <- function(condType = 1L,
                           conjunction = "a",
                           isMore = TRUE,
                           price = 0.0,
                           conId = 0L,
                           exch = "",
                           triggerMethod = 0){
    structure(
      list(condType = condType,
           conjunction = conjunction,
           isMore = isMore,
           price = price,
           conId = conId,
           exch = exch,
           triggerMethod = triggerMethod
           ), class = "OrderCondition"
      )
}

#' @keywords internal
TimeCondition <- function(condType = 3L,
                          conjunction = "a",
                          isMore = TRUE,
                          time= ""){
  structure(
    list(
      condType = condType,
      conjunction = conjunction,
      isMore = isMore,
      time = time
    ), class = "OrderCondition"
  )

}

#' @keywords internal
MarginCondition <- function(condType = 4L,
                            conjunction = "a",
                            isMore = TRUE,
                            percent = 0){
  structure(
    list(
      condType = condType,
      conjunction = conjunction,
      isMore = isMore,
      percent = percent
    ), class = "OrderCondition"
  )
}

#' @keywords internal
ExecutionCondition <- function(condType = 5L,
                               conjunction = "a",
                               secType = "",
                               exch = "",
                               symbol = ""){
  structure(
    list(
      condType = condType,
      conjunction = conjunction,
      secType = secType,
      exch = exch,
      symbol = symbol
    ), class = "OrderCondition"
  )

}

#' @keywords internal
VolumeCondition <- function(condType = 6L,
                            conjunction = "a",
                            isMore= TRUE,
                            volume = 0L,
                            conId= 0L,
                            exch = ""){
  structure(
    list(
      condType = condType,
      conjunction = conjunction,
      isMore = isMore,
      volume = volume,
      conId = conId,
      exch = exch
    ), class = "OrderCondition"
  )
}

#' @keywords internal
PercentChangeCondition <- function(condType = 7L,
                                   conjunction = "a",
                                   isMore = TRUE,
                                   changePercent = 0,
                                   conId = 0L,
                                   exch = ""){
  structure(
    list(
      condType = condType,
      conjunction = conjunction,
      isMore = isMore,
      changePercent = changePercent,
      conId = conId,
      exch = ""
    ), class = "OrderCondition"
  )
}

#' @keywords internal
OrderCondition <- function(condType
                           ){
condtypes <- data.frame(condType = c(1, 3, 4, 5, 6, 7),
                        condDesc = c("PriceCondition",
                                     "TimeCondition",
                                     "MarginCondition",
                                     "ExecutionCondition",
                                     "VolumeCondition",
                                     "PercentChangeCondition"),
                        stringsAsFactors = FALSE)


switch(condtypes$condDesc[condtypes$condType == condType],
       PriceCondition = PriceCondition(),
       TimeCondition = TimeCondition(),
       MarginCondition = MarginCondition(),
       ExecutionCondition = ExecutionCondition(),
       VolumeCondition = VolumeCondition(),
       PercentChangeCondition = PercentChangeCondition()
       )

}


# https://interactivebrokers.github.io/tws-api/order_conditions.html
# https://github.com/erdewit/ib_insync/blob/master/ib_insync/order.py
