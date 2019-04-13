# Contains all the functions for sending and reading an ib api message.

#' @keywords internal
message_length <- function(message) {
  count_char <- nchar(message)
  msg_length <- sum(count_char) + length(count_char)
  as.integer(msg_length)
}

#'@keywords internal
send_message <- function(message, ib_con) {

  out_length <- message_length(message)

  writeBin(out_length, ib_con$con, endian = "big")
  writeBin(message, ib_con$con)
}

#' @keywords internal
parse_incoming_message <- function(connection) {
  # read message length and raw message length
  in_msg_length <- readBin(connection, "integer", endian = "big") # message length
  in_msg <- readBin(connection, "raw", n = in_msg_length)

  # transform message into vector
  in_msg_nuls <- length(in_msg[in_msg == 00])
  readBin(in_msg, "character", n = in_msg_nuls)
}
