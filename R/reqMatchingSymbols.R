#' Requests matching stock symbols.
#'
#' Stock Contract Search.
#'
#' The input can be either the first few letters of the ticker symbol,
#' or for longer strings, a character sequence matching a word in the security
#' name. For instance to search for the stock symbol 'IBKR',
#' the input 'I' or 'IB' can be used, as well as the word 'Interactive'.
#' Up to 16 matching results are returned.
#'
#' @param reqId id to specify the request. Should be a scalar integer.
#' @param pattern Either start of ticker symbol or (for larger strings) company name
#' @param ib_con a valid ib_con connection object
#'
#' @return A data.frame containing sample contract descriptions.
#' @export
#'
#' @examples
#' \dontrun{
#' ib_con <- ib_connect()
#' reqMatchingSymbols(pattern = "Intel", ib_con = ib_con)
#' }
reqMatchingSymbols <- function(reqId = 1L, pattern, ib_con) {
  #TODO: is connected
  #TODO: check pattern


  if (ib_con$server_version < .server_version$MIN_SERVER_VER_REQ_MATCHING_SYMBOLS)
    stop(glue("Current server version {ib_con$server_version} does not support matching symbols request."),
         call. = FALSE)


  out_msg <- c(.outgoing_msg_id$REQ_MATCHING_SYMBOLS,
               make_field(reqId),
               make_field(pattern))

  send_message(out_msg, ib_con)

  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$SYMBOL_SAMPLES) {
        SymbolSamples <- ProcessMsg(in_msg, ib_con)
      } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }

  return(SymbolSamples)
}

processSymbolSamplesMsg <- function(msg, msg_counter) {

  reqId <- as.integer(msg[msg_counter()])
  nContractDescriptions <- as.integer(msg[msg_counter()])

  ContractDescriptionList <- list()

  for (i in seq_len(nContractDescriptions)) {
    contract <- list()

    # read contract fields
    contract$conId <- as.integer(msg[msg_counter()])
    contract$symbol <- msg[msg_counter()]
    contract$secType <- msg[msg_counter()]
    contract$primaryExchange <- msg[msg_counter()]
    contract$currency <- msg[msg_counter()]

    # read derivative sec Types list
    nDerivativeSecTypes <- as.integer(msg[msg_counter()])
    derivativeSecTypes <- ""

    if (nDerivativeSecTypes > 0) {
      for (j in seq_len(nDerivativeSecTypes)) {
        derivativeSecTypes <- paste(derivativeSecTypes, msg[msg_counter()])
      }
      derivativeSecTypes <- glue::trim(derivativeSecTypes)
    }

    ContractDescriptionList[[i]] <- ibapir:::ContractDescription(contract, derivativeSecTypes)
  }

  # transform into data.frame
  data.frame(t(sapply(ContractDescriptionList, unlist)), stringsAsFactors = FALSE)
}
