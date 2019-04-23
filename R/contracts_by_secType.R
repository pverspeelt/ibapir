#' Create a stock contract
#'
#' @param symbol
#' @param exchange
#' @param primaryExchange
#' @param currency
#'
#' @return returns an object of class "contract"
#' @family Contracts
#' @export
#'
#' @examples
#' Example to follow
Stock <- function(symbol,
                  exchange = "SMART",
                  primaryExchange = "",
                  currency = "USD"){

  Contract(symbol = symbol,
           secType = "STK",
           exchange = exchange,
           primaryExchange = primaryExchange,
           currency = currency)
}


#' Create an option contract
#'
#' @param symbol
#' @param lastTradeDateOrContractMonth
#' @param strike
#' @param right
#' @param multiplier
#' @param exchange
#' @param primaryExchange
#' @param currency
#'
#' @return returns an object of class "contract"
#' @family Contracts
#' @export
#'
#' @examples
#' Example to follow
Option <- function(symbol,
                   lastTradeDateOrContractMonth = "",
                   strike = 0,
                   right = "",
                   multiplier = "100",
                   exchange = "SMART",
                   primaryExchange = "",
                   currency = "USD"
) {

  Contract(symbol = symbol,
           secType = "OPT",
           lastTradeDateOrContractMonth = lastTradeDateOrContractMonth,
           strike = strike,
           right = right,
           exchange = exchange,
           primaryExchange = primaryExchange,
           currency = currency
  )
}
