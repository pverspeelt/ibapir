#' Request managed accounts list
#'
#' Important: whenever your TWS user name handles more than a single account,
#' you will be forced to specify the account Id to which the order needs
#' to be allocated. Failure to do so will result in the order being rejected
#' since TWS cannot assign the order to a default account.
#'
#' @param ib_con a valid ib_con object.
#'
#' @return prints the managed accounts that a TWS user name handles when connected to TWS.
#' @export
#'
#' @examples
#' example here
reqManagedAccts <- function(ib_con) {

  if (!is_ib_connection(ib_con)) {
    stop("not a valid ib connection", call. = FALSE)
  }

  VERSION <- "1"
  out_msg <- c(.outgoing_msg_id$REQ_MANAGED_ACCTS,
               VERSION)

  send_message(out_msg, ib_con)

  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {

      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$MANAGED_ACCTS) {
        managed_accounts <- ProcessMsg(in_msg, ib_con)
      } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }

  if (length(managed_accounts) > 1) {
    print(glue('Your TWS user name handles {length(managed_accounts)} accounts.',
               ' Read the documentation with the req_managed_accounts function.'))
  }
  print(glue("you are connected to account: {managed_accounts}"))
  return(managed_accounts)
}

#' @keywords internal
processManagedAcctsMsg <- function(msg, msg_counter){
  version <- as.integer(msg[msg_counter()])
  accounts <- unlist(strsplit(msg[msg_counter()], split = ",", fixed = TRUE))
}
