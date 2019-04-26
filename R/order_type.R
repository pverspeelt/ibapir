#' Create a limit order object
#'
#' @param action Generally available values are "BUY" and "SELL".
#' Additionally, "SSHORT", "SLONG" are available in some institutional-accounts only
#' @param totalQuantity The number of positions being bought/sold.
#' @param lmtPrice The LIMIT price. Used for limit, stop-limit and relative orders
#'
#' @return Returns a limit order object for the placeOrder function.
#' @export
#'
#' @examples
#' Example to follow
LimitOrder <- function(action, totalQuantity, lmtPrice) {
  Order(action = action,
        totalQuantity = totalQuantity,
        orderType = "LMT",
        lmtPrice = lmtPrice)
}
