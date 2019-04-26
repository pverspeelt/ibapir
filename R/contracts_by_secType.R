#' Create a stock contract
#'
#' @param symbol The contract (or its underlying) symbol.
#' @param exchange The destination exchange.
#' @param primaryExchange The contract's primary exchange.
#' @param currency The underlying's currency.
#'
#' @return returns an object of class "Contract"
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
#' @param lastTradeDateOrContractMonth The contract's last trading day or
#' contract month.
#' @param strike The option's strike price.
#' @param right Put or Call. Valid values are 'P', 'PUT', 'C', 'CALL'
#' @param multiplier The instrument's multiplier. Default value is: 100
#' @inheritParams Stock
#'
#' @return returns an object of class "Contract"
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
