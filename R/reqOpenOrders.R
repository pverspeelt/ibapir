#' Title
#'
#' Requests all open orders places by this specific API client
#' (identified by the API client id). For client ID 0, this will bind
#' previous manual TWS orders.
#'
#' @param ib_con A valid ib connection.
#'
#' @return a data.frame with all open orders
#' @export
#'
#' @examples
#' Example to follow
reqOpenOrders <- function(ib_con){

  VERSION = 1

  out_msg = c(make_field(.outgoing_msg_id$REQ_OPEN_ORDERS),
              make_field(VERSION))

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
