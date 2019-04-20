#' Request next valid Id
#'
#' Requests the next valid order ID at the current moment.
#' Important: the next valid order ID is only valid at the time it is received.
#'
#' @param ib_con a valid ib_con connection object.
#'
#' @return returns the next valid Id.
#' @export
#'
#' @examples
#' \dontrun{
#' ib_con <- ib_connect()
#' reqIds(ib_con)
#' }
reqIds <- function(ib_con) {

  if (!is_ib_connection(ib_con)) {
    stop("not a valid ib connection", call. = FALSE)
  }

  VERSION <- "1"
  numIds <- 1L

  out_msg <- c(.outgoing_msg_id$REQ_IDS,
               VERSION,
               make_field(numIds))

  send_message(out_msg, ib_con)

  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$NEXT_VALID_ID) {
        nextValidID <- ProcessMsg(in_msg, ib_con)
      } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }
  return(nextValidID)
}


#' @keywords internal
processNextValidIdMsg <- function(msg, msg_counter) {
  version <- as.integer(msg[msg_counter()])
  nextValidID <- as.integer(msg[msg_counter()])
}
