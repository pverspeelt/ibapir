#' Request contract details
#'
#' @param contract A valid contract. You can create a valid contract with the
#' \code{\link{Contract}} function or one of the contract helper functions.
#' @param reqId The request's unique identifier. Should be a scalar integer.
#' @param ib_con A valid ib connection.
#'
#' @return A list of details for the specified contract. Updates nextValidId in
#' the ib connection.
#' @export
#'
#' @examples
#' \dontrun{
#' ib_con <- ib_connect()
#' ibm <- Contract(symbol = "IBM",
#'                 secType = "STK",
#'                 currency = "USD",
#'                 exchange = "SMART")
#' ibm_details <- reqContractDetails(ibm, reqId = 1L, ib_con = ib_con)
#' }
reqContractDetails <- function(contract, reqId = 1L, ib_con) {

  # create and send outgoing message ----------
  out_msg <- create_reqContractDetails_msg(contract = contract,
                                           reqId = reqId,
                                           ib_con = ib_con)
  send_message(out_msg, ib_con)

  contracts <- list()
  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$CONTRACT_DATA) {
        contracts[[length(contracts) + 1L]] <- ProcessMsg(in_msg, ib_con)
      } else if (msgId == .incoming_msg_id$CONTRACT_DATA_END) {
          ProcessMsg(in_msg, ib_con)
        } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }

  return(contracts)
}


#' @keywords internal
create_reqContractDetails_msg <- function(contract, reqId, ib_con) {

  VERSION = 8

  out_msg <- c(.outgoing_msg_id$REQ_CONTRACT_DATA,
               make_field(VERSION),
               make_field(reqId),
               make_field(contract$conId),
               make_field(contract$symbol),
               make_field(contract$secType),
               make_field(contract$lastTradeDateOrContractMonth),
               make_field(contract$strike),
               make_field(contract$right),
               make_field(contract$multiplier),
               make_field(contract$exchange),
               make_field(contract$primaryExchange),
               make_field(contract$currency),
               make_field(contract$localSymbol),
               make_field(contract$tradingClass),
               make_field(contract$includeExpired),
               make_field(contract$secIdType),
               make_field(contract$secId))
}

#' @keywords internal
processContractDataMsg <- function(msg, msg_counter, ib_con) {
  version <- as.integer(msg[msg_counter()])

  reqId <- msg[msg_counter()]
  if (version >= 3) {
    reqId <- as.integer(reqId)
  } else reqId <- -1

  contract <- ContractDetails(Contract())
  contract$contract$symbol <- msg[msg_counter()]
  contract$contract$secType <- msg[msg_counter()]
  # decoder function available for lastTradeDateOrContractMonth
  contract$contract$lastTradeDateOrContractMonth <- msg[msg_counter()]
  contract$contract$strike <- as.numeric(msg[msg_counter()])
  contract$contract$right <- msg[msg_counter()]
  contract$contract$exchange <- msg[msg_counter()]
  contract$contract$currency <- msg[msg_counter()]
  contract$contract$localSymbol <- msg[msg_counter()]
  contract$marketName <- msg[msg_counter()]
  contract$contract$tradingClass <- msg[msg_counter()]
  contract$contract$conId <- as.integer(msg[msg_counter()])
  contract$minTick <- as.numeric(msg[msg_counter()])

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_MD_SIZE_MULTIPLIER) {
    contract$mdSizeMultiplier <- as.integer(msg[msg_counter()])
  }

  contract$contract$multiplier <- msg[msg_counter()]
  contract$orderTypes <- msg[msg_counter()]
  contract$validExchanges <- msg[msg_counter()]
  contract$priceMagnifier <- as.integer(msg[msg_counter()]) # ver 2 field

  if (version >= 4) {
    contract$underConId <- as.integer(msg[msg_counter()])
  }

  if (version >= 5) {
    contract$longName <- msg[msg_counter()]
    contract$contract$primaryExchange <- msg[msg_counter()]
  }

  if (version >= 6) {
    contract$contractMonth <- msg[msg_counter()]
    contract$industry <- msg[msg_counter()]
    contract$category <- msg[msg_counter()]
    contract$subcategory <- msg[msg_counter()]
    contract$timeZoneId <- msg[msg_counter()]
    contract$tradingHours <- msg[msg_counter()]
    contract$liquidHours <- msg[msg_counter()]
  }

  if (version >= 8) {
    contract$evRule <- msg[msg_counter()]
    contract$evMultiplier <- as.integer(msg[msg_counter()])
  }

  if (version >= 7) {
    secIdListCount <- as.integer(msg[msg_counter()])
    if (secIdListCount  > 0){
      # create list of secids

      tag <- NULL
      value <- NULL
      for (i in 1:secIdListCount){
        tag[i] <- msg2[msg_counter()]
        value[i] <- msg[msg_counter()]
      }
      contract$secIdList <- list(tag = tag,
                                 value = value)
    }
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_AGG_GROUP) {
    contract$aggGroup <- as.integer(msg[msg_counter()])
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_UNDERLYING_INFO) {
    contract$underSymbol <- msg[msg_counter()]
    contract$underSecType <- msg[msg_counter()]
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_MARKET_RULES) {
    contract$marketRuleIds <- msg[msg_counter()]
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_REAL_EXPIRATION_DATE) {
    contract$realExpirationDate <- msg[msg_counter()]
  }

  return(contract)
}

#' @keywords internal
processContractDataEndMsg <- function(msg, msg_counter, ib_con) {
  version <- as.integer(msg[msg_counter()])
  reqId <- as.integer(msg[msg_counter()])
  print(glue("next valid reqId: {reqId}"))
  ib_con$nextValidId <- reqId
}


#TODO: decoder for lastTradeDateOrContractMonth
# readLastTradeDate <- function(fields, isBond = FALSE) {
#   lastTradeDateOrContractMonth <- fields
#   if (lastTradeDateOrContractMonth != NULL) {
#     splitted = strsplit(lastTradeDateOrContractMonth, split = "\\s+")
#   }
#   if length(splitted) > 1 {
#     if (isBond) {
#   contract$maturity = splitted[1]
#     } else {
#       contract$contract$lastTradeDateOrContractMonth = splitted[1]
#     }
#   }
#
#    if (length(splitted) > 2) {
#     contract$lastTradeTime = splitted[1]
#    }
#
#   if (isBond && length(splitted) > 3){
#     contract$timeZoneId = splitted[3]
#   }
# }
