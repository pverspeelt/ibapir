#' Process incoming messages
#'
#' This function is only used for processing incoming messages.
#'
#' @param in_msg
#' @param ib_con
#'
#' @return used for processing incoming messages. Called for it's side-effects.
#' @keywords internal
#'
ProcessMsg <- function(in_msg, ib_con) {

  msg_counter <- counter()
  msgId <- as.integer(in_msg[msg_counter()])

  switch(names(which(.incoming_msg_id == msgId)),
         ERR_MSG = processErrMsg(in_msg, ib_con, msg_counter),
         MANAGED_ACCTS = processManagedAcctsMsg(in_msg, msg_counter),
         NEXT_VALID_ID = processNextValidIdMsg(in_msg, msg_counter),
         CURRENT_TIME = processCurrentTimeMsg(in_msg, msg_counter),
         warning(glue("Unknown incoming message Id: {msgId}
                      with message: {paste0(in_msg, collapse = \" \")}."), call.=FALSE)
         )
}



#' @keywords internal
processErrMsg <- function(msg, ib_con, msg_counter) {
  version <- as.integer(msg[msg_counter()])
  id <- as.integer(msg[msg_counter()])
  error_code <- as.integer(msg[msg_counter()])
  error_message <- msg[msg_counter()]

  if (error_code == 1100)
    ib_con$connected <- FALSE
  if (error_code %in% c(1101, 1102))
    ib_con$connected <- TRUE
  print(glue("api message: {error_code} - {error_message}"))
}
