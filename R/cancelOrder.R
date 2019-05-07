#' Call this function to cancel an order.
#'
#' Cancels an active order placed by from the same API client ID.
#' Note: API clients cannot cancel individual orders placed by other clients.
#' Only reqGlobalCancel can do this.
#'
#' @param orderId The order ID that was specified previously in the \code{\link{placeOrder}}
#' function
#' @param ib_con A valid ib connection.
#'
#' @export
#'
#' @examples
#' Example to follow
cancelOrder <- function(orderId, ib_con){

  VERSION = 1

  out_msg = c(make_field(.outgoing_msg_id$CANCEL_ORDER),
              make_field(VERSION),
              make_field(orderId))

  send_message(out_msg, ib_con)

}
