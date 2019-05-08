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

  # receive messages
  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      # if (msgId == .incoming_msg_id$ORDER_STATUS) {
      #   current_time <- ProcessMsg(in_msg, ib_con)
      # } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }

}


#' Call this function to cancel all active orders.
#'
#' This method will cancel ALL open orders including those placed directly from TWS.
#'
#' @param ib_con A valid ib connection.
#'
#' @export
#'
#' @examples
#' Example to follow

reqGlobalCancel <- function(ib_con){

  VERSION = 1

  out_msg = c(make_field(.outgoing_msg_id$REQ_GLOBAL_CANCEL),
              make_field(VERSION))

  send_message(out_msg, ib_con)

  # receive messages
  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$ERR_MSG) {
        ProcessMsg(in_msg, ib_con)
      }
    } else
      break
  }
}
