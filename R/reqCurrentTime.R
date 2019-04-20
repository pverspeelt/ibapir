#' Request the current time from the IB server
#'
#' IB's current time. TWS or IB Gateway is synchronized with the server (not local computer)
#' using NTP and this function will receive the current time in TWS.
#'
#' @param ib_con a valid tws connection.
#'
#' @return returns the current time of the IB server
#' @export
#'
#' @examples
#' \dontrun{
#' ib_con <- ib_connect()
#' reqCurrentTime(ib_con)
#' }
reqCurrentTime <- function(ib_con) {

  if (!is_ib_connection(ib_con)) {
    stop("not a valid ib connection", call. = FALSE)
  }


  VERSION <- "1"

  out_msg <- c(.outgoing_msg_id$REQ_CURRENT_TIME,
               VERSION)

  send_message(out_msg, ib_con)

  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$CURRENT_TIME) {
        current_time <- ProcessMsg(in_msg, ib_con)
      } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }
  return(current_time)
}

#' @keywords internal
processCurrentTimeMsg <- function(msg, msg_counter){
  version <- as.integer(msg[msg_counter()])
  current_time <- structure(as.numeric(msg[msg_counter()]), class="POSIXct")
}
