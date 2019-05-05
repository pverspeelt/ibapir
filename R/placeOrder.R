#' Place an Order
#'
#' Call this function to place an order. The order status will
#' be returned by the orderStatus event.
#'
#' @param orderId The order id. You must specify a unique value. When the
#' order START_API returns, it will be identified by this tag. This tag is also
#' used when canceling the order.
#' @param contract A valid contract. You can create a valid contract with the
#' \code{\link{Contract}} function or one of the contract helper functions.
#' @param order A valid Order. You can create a valid order with the
#' \code{\link{Order}} function or one of the order type functions.
#' @param ib_con A valid ib connection.
#'
#' @return Returns an order status message
#' @export
#'
#' @examples
#' Example to follow.
placeOrder <- function(orderId, contract, order, ib_con){

  out_msg <- create_placeOrder_msg(orderId, contract, order, ib_con)


}


create_placeOrder_msg <- function(orderId, contract, order, ib_con) {

  # server version checks ----------
  if (ib_con$server_version < .server_version$MIN_SERVER_VER_MODELS_SUPPORT &&
      nchar(order$modelCode) != 0) {
    stop(glue("Current server version {ib_con$server_version} does not support model code parameter."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_EXT_OPERATOR &&
      nchar(order$extOperator) != 0) {
    stop(glue("Current server version {ib_con$server_version} does not support ext operator parameter."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_SOFT_DOLLAR_TIER &&
      (nchar(order$softDollarTier$name) != 0 || nchar(order$softDollarTier$val) != 0)) {
    stop(glue("Current server version {ib_con$server_version} does not support soft dollar tier."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_CASH_QTY &&
      length(order$cashQty) != 0) {
    stop(glue("Current server version {ib_con$server_version} does not support cash quantity parameter."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_DECISION_MAKER &&
      (order$mifid2DecisionMaker != "" || order$mifid2DecisionAlgo != "")) {
    stop(glue("Current server version {ib_con$server_version} does not support MIFID II decision maker parameters."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_MIFID_EXECUTION &&
      (order$mifid2ExecutionTrader != "" || order$mifid2ExecutionAlgo != "")) {
    stop(glue("Current server version {ib_con$server_version} does not support MIFID II execution parameters."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE &&
     order$dontUseAutoPriceForHedge != FALSE) {
    stop(glue("Current server version {ib_con$server_version} does not support dontUseAutoPriceForHedge parameter."),
         call. = FALSE)
  }

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_ORDER_CONTAINER &&
      order$isOmsContainer != FALSE) {
    stop(glue("Current server version {ib_con$server_version} does not support oms container parameter."),
         call. = FALSE)
  }

  # create outgoing message ----------
  VERSION = 45L

  out_msg <- .outgoing_msg_id$PLACE_ORDER

  # server version 145 and higher do not send the Version field anymore.
  if (ib_con$server_version < .server_version$MIN_SERVER_VER_ORDER_CONTAINER) {
    out_msg <- c(out_msg,
                 make_field(VERSION))
  }

  out_msg <- c(out_msg,
               make_field(orderId),
               # contract fields
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
               make_field(contract$secIdType),
               make_field(contract$secId),
               # main order fields
               make_field(order$action)
               )

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_FRACTIONAL_POSITIONS) {
    out_msg <- c(out_msg,
                 make_field(order$totalQuantity))
   } else {
    out_msg <- c(out_msg,
                 make_field(as.integer(order$totalQuantity)))
   }

  out_msg <- c(out_msg,
               make_field(order$orderType),
               make_field(order$lmtPrice),
               make_field(order$auxPrice),

               # extended order in_msg[msg_counter()]
               make_field(order$tif),
               make_field(order$ocaGroup),
               make_field(order$account),
               make_field(order$openClose),
               make_field(order$origin),
               make_field(order$orderRef),
               make_field(order$transmit),
               make_field(order$parentId),
               make_field(order$blockOrder),
               make_field(order$sweepToFill),
               make_field(order$displaySize),
               make_field(order$triggerMethod),
               make_field(order$outsideRth),
               make_field(order$hidden)
               )


  # BAG order fields
  if (contract$secType == "BAG") {
    comboLegsCount <- ifelse(is.null(contract$comboLegs),
                             0,
                             length(contract$comboLegs))
    out_msg <- c(out_msg,
                 make_field(comboLegsCount))

    if (comboLegsCount > 0) {
      for(i in seq_len(comboLegsCount)) {
        comboLeg <- contract$comboLegs[[i]]
        out_msg <- c(out_msg,
                     make_field(comboLeg$conId),
                     make_field(comboLeg$ratio),
                     make_field(comboLeg$action),
                     make_field(comboLeg$exchange),
                     make_field(comboLeg$openClose),
                     make_field(comboLeg$shortSaleSlot),
                     make_field(comboLeg$designatedLocation),
                     make_field(comboLeg$exemptCode))
      }

      orderComboLegsCount <- ifelse(is.null(order$orderComboLegs),
                                   0,
                                   length(order$orderComboLegs))
      out_msg <- c(out_msg,
                   make_field(orderComboLegsCount))
    }

    if (orderComboLegsCount > 0) {
      for(i in seq_len(orderComboLegsCount)) {
        orderComboLeg <- order$orderComboLegs[[i]]
        out_msg <- c(out_msg,
                     make_field(orderComboLeg$price))
      }
    }

    smartComboRoutingParamsCount <- ifelse(is.null(order$smartComboRoutingParams),
                                          0,
                                          length(order$smartComboRoutingParams))
    out_msg <- c(out_msg,
                 make_field(smartComboRoutingParamsCount))

    if (smartComboRoutingParamsCount > 0) {
      for (i in seq_len(smartComboRoutingParamsCount)) {
        tagValue <- order$smartComboRoutingParams[[i]]
        out_msg <- c(out_msg,
                     make_field(tagValue$tag),
                     make_field(tagValue$value))
      }
    }
  }
  # end BAG order fields

  out_msg <- c(out_msg,
               "", # deprecated sharesAllocation field
               make_field(order$discretionaryAmt),
               make_field(order$goodAfterTime),
               make_field(order$goodTillDate),
               make_field(order$faGroup),
               make_field(order$faMethod),
               make_field(order$faPercentage),
               make_field(order$faProfile))

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_MODELS_SUPPORT) {
    out_msg <- c(out_msg,
                 make_field(order$modelCode))
  }

  out_msg <- c(out_msg,
               make_field(order$shortSaleSlot), # 0 for retail, 1 or 2 for institutions
               make_field(order$designatedLocation),# populate only when shortSaleSlot = 2.
               make_field(order$exemptCode),
               make_field(order$ocaType),
               make_field(order$rule80A),
               make_field(order$settlingFirm),
               make_field(order$allOrNone),
               make_field(order$minQty),
               make_field(order$percentOffset),
               make_field(order$eTradeOnly),
               make_field(order$firmQuoteOnly),
               make_field(order$nbboPriceCap),
               make_field(order$auctionStrategy),
               make_field(order$startingPrice),
               make_field(order$stockRefPrice),
               make_field(order$delta),
               make_field(order$stockRangeLower),
               make_field(order$stockRangeUpper),
               make_field(order$overridePercentageConstraints),

               # Volatility orders
               make_field(order$volatility),
               make_field(order$volatilityType),
               make_field(order$deltaNeutralOrderType),
               make_field(order$deltaNeutralAuxPrice)
               )

  if (nchar(order$deltaNeutralOrderType) != 0) {
    out_msg <- c(out_msg,
                 make_field(order$deltaNeutralConId),
                 make_field(order$deltaNeutralSettlingFirm),
                 make_field(order$deltaNeutralClearingAccount),
                 make_field(order$deltaNeutralClearingIntent),
                 make_field(order$deltaNeutralOpenClose),
                 make_field(order$deltaNeutralShortSale),
                 make_field(order$deltaNeutralShortSaleSlot),
                 make_field(order$deltaNeutralDesignatedLocation)
                 )
  }

  out_msg <- c(out_msg,
               make_field(order$continuousUpdate),
               make_field(order$referencePriceType),
               make_field(order$trailStopPrice),
               make_field(order$trailingPercent),

               # SCALE orders
               make_field(order$scaleInitLevelSize),
               make_field(order$scaleSubsLevelSize),
               make_field(order$scalePriceIncrement)
               )

  if (length(order$scalePriceIncrement) > 0 &&
      order$scalePriceIncrement > 0) {
    out_msg <- c(out_msg,
                 make_field(order$scalePriceAdjustValue),
                 make_field(order$scalePriceAdjustInterval),
                 make_field(order$scaleProfitOffset),
                 make_field(order$scaleAutoReset),
                 make_field(order$scaleInitPosition),
                 make_field(order$scaleInitFillQty),
                 make_field(order$scaleRandomPercent)
                 )
  }

  out_msg <- c(out_msg,
               make_field(order$scaleTable),
               make_field(order$activeStartTime),
               make_field(order$activeStopTime),
               # HEDGE orders
               make_field(order$hedgeType)
               )

  if (nchar(order$hedgeType) > 0) {
    out_msg <- c(out_msg,
                 make_field(order$hedgeParam))
  }

  out_msg <- c(out_msg,
               make_field(order$optOutSmartRouting),
               make_field(order$clearingAccount),
               make_field(order$clearingIntent),
               make_field(order$notHeld)
               )

  # delta neutral contract
  if (!is.null(contract$deltaNeutralContract)) {
    out_msg <- c(cout_msg,
                 make_field(TRUE),
                 make_field(contract$deltaNeutralContract$conId),
                 make_field(contract$deltaNeutralContract$delta),
                 make_field(contract$deltaNeutralContract$price)
                 )
  } else {
    out_msg <- c(out_msg,
                 make_field(FALSE))
  }

  # Algo strategy
  out_msg <- c(out_msg,
               make_field(order$algoStrategy))

  if (nchar(order$algoStrategy) != 0) {
    algoParamsCount <- ifelse(is.null(order$algoParams),
                              0,
                              length(order$algoParams))
    out_msg <- c(out_msg,
                 make_field(algoParamsCount))

    if (algoParamsCount > 0) {
      for(i in seq_len(algoParamsCount)){
        algoParam <- order$algoParams[[i]]
        out_msg <- c(out_msg,
                     make_field(algoParam$tag),
                     make_field(algoParam$value)
                     )
      }
    }
  }

  out_msg <- c(out_msg,
               make_field(order$algoId),
               make_field(order$whatIf),
               make_field(order$orderMiscOptions),
               make_field(order$solicited),
               make_field(order$randomizeSize),
               make_field(order$randomizePrice)
               )

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_PEGGED_TO_BENCHMARK) {


    if (order$orderType == "PEG BENCH") {
    out_msg <- c(out_msg,
                 make_field(order$referenceContractId),
                 make_field(order$isPeggedChangeAmountDecrease),
                 make_field(order$peggedChangeAmount),
                 make_field(order$referenceChangeAmount),
                 make_field(order$referenceExchangeId)
                 )
    }

    out_msg <- c(out_msg,
                 make_field(length(order$conditions)))

    # TODO test order conditions
    if (length(order$conditions) > 0) {
      for(i in seq_along(order$conditions)) {
        cond <- order$conditions[[i]]
        out_msg <- c(out_msg,
                     make_field(cond$type)
                     )
      }

      out_msg <- c(out_msg,
                   make_field(order$conditionsIgnoreRth),
                   make_field(order$conditionsCancelOrder))
    }

    out_msg <- c(out_msg,
                 make_field(order$adjustedOrderType),
                 make_field(order$triggerPrice),
                 make_field(order$lmtPriceOffset),
                 make_field(order$adjustedStopPrice),
                 make_field(order$adjustedStopLimitPrice),
                 make_field(order$adjustedTrailingAmount),
                 make_field(order$adjustableTrailingUnit)
                 )
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_EXT_OPERATOR) {
    out_msg <- c(out_msg,
                 make_field(order$extOperator))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_SOFT_DOLLAR_TIER) {
    out_msg <- c(out_msg,
                 make_field(order$softDollarTier$name),
                 make_field(order$softDollarTier$val))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_CASH_QTY) {
    out_msg <- c(out_msg,
                 make_field(order$cashQty))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_DECISION_MAKER) {
    out_msg <- c(out_msg,
                 make_field(order$mifid2DecisionMaker),
                 make_field(order$mifid2DecisionAlgo))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_MIFID_EXECUTION) {
    out_msg <- c(out_msg,
                 make_field(order$mifid2ExecutionTrader),
                 make_field(order$mifid2ExecutionAlgo))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE) {
    out_msg <- c(out_msg,
                 make_field(order$dontUseAutoPriceForHedge))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_ORDER_CONTAINER) {
    out_msg <- c(out_msg,
                 make_field(order$isOmsContainer))
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_D_PEG_ORDERS) {
    out_msg <- c(out_msg,
                 make_field(order$discretionaryUpToLimitPrice))
  }

  # New API version
  # if (ib_con$server_version >= .server_version$MIN_SERVER_VER_PRICE_MGMT_ALGO) {
  #   out_msg <- c(out_msg,
  #                make_field(order$usePriceMgmtAlgo))
  # }

  return(out_msg)
}


processOrderStatusMsg <- function(in_msg, msg_counter, ib_con) {

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_MARKET_CAP_PRICE){
   version <- as.integer(in_msg[msg_counter()])
  }

  orderId <- as.integer(in_msg[msg_counter()])
  status <- in_msg[msg_counter()]

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_FRACTIONAL_POSITIONS) {
    filled <- as.numeric(in_msg[msg_counter()])
  } else {
    filled <- as.integer(in_msg[msg_counter()])
  }

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_FRACTIONAL_POSITIONS) {
    remaining <- as.numeric(in_msg[msg_counter()])
  } else {
    remaining <- as.integer(in_msg[msg_counter()])
  }

  avgFillPrice <- as.numeric(in_msg[msg_counter()])

  permId <- as.integer(in_msg[msg_counter()])
  parentId <- as.integer(in_msg[msg_counter()])
  lastFillPrice <- as.numeric(in_msg[msg_counter()])
  clientId <- as.integer(in_msg[msg_counter()])
  whyHeld <- in_msg[msg_counter()]

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_MARKET_CAP_PRICE) {
    mktCapPrice <- as.numeric(in_msg[msg_counter()])
  } else {
    mktCapPrice <- NULL
  }

  orderStatus(orderId, status, filled, remaining, avgFillPrice,
              permId, parentId, lastFillPrice, clientId, whyHeld,
              mktCapPrice)
}


orderStatus <- function(orderId, status, filled, remaining, avgFillPrice,
                        permId, parentId, lastFillPrice, clientId, whyHeld,
                        mktCapPrice) {

  data.frame(orderId = orderId,
             status = status,
             filled= filled,
             remaining = remaining,
             avgFillPrice = avgFillPrice,
             permId = permId,
             parentId  = parentId,
             lastFillPrice = lastFillPrice,
             clientId = clientId,
             whyHeld = whyHeld,
             mktCapPrice = mktCapPrice,
             stringsAsFactors = FALSE)

}


processOpenOrderMsg <- function(in_msg, msg_counter, ib_con) {

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_ORDER_CONTAINER) {
    version <- as.integer(in_msg[msg_counter()])
  } else {
    version = ib_con$server_version
  }

  order <- Order()
  order$orderId <- as.integer(in_msg[msg_counter()])

  contract <- Contract()

  # read contract in_msg[msg_counter()]
  contract$conId <- as.integer(in_msg[msg_counter()])
  contract$symbol <- in_msg[msg_counter()]
  contract$secType <- in_msg[msg_counter()]
  contract$lastTradeDateOrContractMonth <- in_msg[msg_counter()]
  contract$strike <- as.numeric(in_msg[msg_counter()])
  contract$right <- in_msg[msg_counter()]

  if (version >= 32) {
    contract$multiplier <- in_msg[msg_counter()]
  }

  contract$exchange <- in_msg[msg_counter()]
  contract$currency <- in_msg[msg_counter()]
  contract$localSymbol <- in_msg[msg_counter()]

  if (version >= 32) {
    contract$tradingClass <- in_msg[msg_counter()]
  }

  # read order in_msg[msg_counter()]
  order$action <- in_msg[msg_counter()]

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_FRACTIONAL_POSITIONS){
    order$totalQuantity <- as.numeric(in_msg[msg_counter()])
  } else {
    order$totalQuantity <- as.integer(in_msg[msg_counter()])
  }

  order$orderType = in_msg[msg_counter()]

  if (version < 29) {
    order$lmtPrice = as.numeric(in_msg[msg_counter()])
  } else {
    order$lmtPrice = decoder("numeric", in_msg[msg_counter()]) # show_unset
  }
  if (version < 30){
    order$auxPrice = as.numeric(in_msg[msg_counter()])
  } else {
    order$auxPrice = decoder("numeric", in_msg[msg_counter()]) # show_unset
  }

  order$tif = in_msg[msg_counter()]
  order$ocaGroup = in_msg[msg_counter()]
  order$account = in_msg[msg_counter()]
  order$openClose = in_msg[msg_counter()]

  order$origin = as.integer(in_msg[msg_counter()])

  order$orderRef = in_msg[msg_counter()]
  order$clientId = as.integer(in_msg[msg_counter()])
  order$permId = decoder("integer", in_msg[msg_counter()])

  order$outsideRth = decoder("bool", in_msg[msg_counter()])
  order$hidden = decoder("bool", in_msg[msg_counter()])
  order$discretionaryAmt = decoder("numeric", in_msg[msg_counter()])
  order$goodAfterTime = in_msg[msg_counter()]

  sharesAllocation = in_msg[msg_counter()] # deprecated field

  order$faGroup = in_msg[msg_counter()]
  order$faMethod = in_msg[msg_counter()]
  order$faPercentage = in_msg[msg_counter()]
  order$faProfile = in_msg[msg_counter()]

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_MODELS_SUPPORT) {
    order$modelCode = in_msg[msg_counter()]
  }

  order$goodTillDate = in_msg[msg_counter()]

  order$rule80A = in_msg[msg_counter()]
  order$percentOffset = decoder("numeric", in_msg[msg_counter()]) # show_unset
  order$settlingFirm = in_msg[msg_counter()]
  order$shortSaleSlot = decoder("integer", in_msg[msg_counter()])
  order$designatedLocation = in_msg[msg_counter()]

  if (version >= 23) {
    order$exemptCode = decoder("integer", in_msg[msg_counter()])
  }

  order$auctionStrategy = decoder("integer", in_msg[msg_counter()])
  order$startingPrice = decoder("numeric", in_msg[msg_counter()]) # show_unset
  order$stockRefPrice = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  order$delta = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  order$stockRangeLower = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  order$stockRangeUpper = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  order$displaySize = decoder("integer", in_msg[msg_counter()])

  order$blockOrder = decoder("bool", in_msg[msg_counter()])
  order$sweepToFill = decoder("bool", in_msg[msg_counter()])
  order$allOrNone = decoder("bool", in_msg[msg_counter()])
  order$minQty = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
  order$ocaType = decoder("integer", in_msg[msg_counter()])
  order$eTradeOnly = decoder("bool", in_msg[msg_counter()])
  order$firmQuoteOnly = decoder("bool", in_msg[msg_counter()])
  order$nbboPriceCap = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET

  order$parentId = decoder("integer", in_msg[msg_counter()])
  order$triggerMethod = decoder("integer", in_msg[msg_counter()])

  order$volatility = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  order$volatilityType = decoder("integer", in_msg[msg_counter()])
  order$deltaNeutralOrderType = in_msg[msg_counter()]
  order$deltaNeutralAuxPrice = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET

  if (version >= 27 && nchar(order$deltaNeutralOrderType) > 0){
    order$deltaNeutralConId = decoder("integer", in_msg[msg_counter()])
    order$deltaNeutralSettlingFirm = in_msg[msg_counter()]
    order$deltaNeutralClearingAccount = in_msg[msg_counter()]
    order$deltaNeutralClearingIntent = in_msg[msg_counter()]
  }

  if (version >= 31 && nchar(order$deltaNeutralOrderType)){
    order$deltaNeutralOpenClose = in_msg[msg_counter()]
    order$deltaNeutralShortSale = decoder("bool", in_msg[msg_counter()])
    order$deltaNeutralShortSaleSlot = decoder("integer", in_msg[msg_counter()])
    order$deltaNeutralDesignatedLocation = in_msg[msg_counter()]
  }

  order$continuousUpdate = decoder("bool", in_msg[msg_counter()])

  order$referencePriceType = decoder("integer", in_msg[msg_counter()])

  order$trailStopPrice = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET

  if (version >= 30){
    order$trailingPercent = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  }

  order$basisPoints = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  order$basisPointsType = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
  contract$comboLegsDescrip = in_msg[msg_counter()]

  # comboleg fields
  if (version >= 29){
    comboLegsCount = decoder("integer", in_msg[msg_counter()])

      if (comboLegsCount > 0){
        contract$comboLegs = list()
        for (i in seq_len(comboLegsCount)){
        comboLeg = ComboLeg()
        comboLeg$conId = decoder("integer", in_msg[msg_counter()])
        comboLeg$ratio = decoder("integer", in_msg[msg_counter()])
        comboLeg$action = in_msg[msg_counter()]
        comboLeg$exchange = in_msg[msg_counter()]
        comboLeg$openClose = decoder("integer", in_msg[msg_counter()])
        comboLeg$shortSaleSlot = decoder("integer", in_msg[msg_counter()])
        comboLeg$designatedLocation = in_msg[msg_counter()]
        comboLeg$exemptCode = decoder("integer", in_msg[msg_counter()])
        contract$comboLegs[i] <- comboLeg
        }
      }


      orderComboLegsCount = decoder("integer", in_msg[msg_counter()])
      if (orderComboLegsCount > 0){
        order$orderComboLegs = list()
        for (i in seq_len(orderComboLegsCount)){
          orderComboLeg = OrderComboLeg()
          orderComboLeg$price = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
          order$orderComboLegs[i] <- orderComboLeg
        }
      }
  }
  # end comboleg fields

  if (version >= 26){
    smartComboRoutingParamsCount = decoder("integer", in_msg[msg_counter()])
    if (smartComboRoutingParamsCount > 0) {
      order$smartComboRoutingParams = list()
      for (i in seq_len(smartComboRoutingParamsCount)) {
        tagValue = TagValue()
        tagValue.tag = decode(str, in_msg[msg_counter()])
        tagValue.value = decode(str, in_msg[msg_counter()])
        order$smartComboRoutingParams[i] <- tagValue
        }
    }
  }

  if (version >= 20){
    order$scaleInitLevelSize = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
    order$scaleSubsLevelSize = decoder("integer", in_msg[msg_counter()]) #  SHOW_UNSET
    } else {# ver 15 fields
    order$notSuppScaleNumComponents = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
    order$scaleInitLevelSize = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET # scaleComponectSize
  }

  order$scalePriceIncrement = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET # ver 15 field

  if (version >= 28 &&
      !is.na(order$scalePriceIncrement) && # order$scalePriceIncrement != UNSET_DOUBLE
      order$scalePriceIncrement > 0.0) {
    order$scalePriceAdjustValue = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
    order$scalePriceAdjustInterval = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
    order$scaleProfitOffset = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
    order$scaleAutoReset = decoder("bool", in_msg[msg_counter()])
    order$scaleInitPosition = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
    order$scaleInitFillQty = decoder("integer", in_msg[msg_counter()]) # SHOW_UNSET
    order$scaleRandomPercent = decoder("bool", in_msg[msg_counter()])
  }

  if (version >= 24) {
    order$hedgeType = in_msg[msg_counter()]
    if (nchar(order$hedgeType) > 0) {
      order$hedgeParam = in_msg[msg_counter()]
    }
  }


  if (version >= 25){
    order$optOutSmartRouting = decoder("bool", in_msg[msg_counter()])
    }

  order$clearingAccount = in_msg[msg_counter()]
  order$clearingIntent = in_msg[msg_counter()]

  if (version >= 22) {
    order$notHeld = decoder("bool", in_msg[msg_counter()])
  }


  if (version >= 20){
    deltaNeutralContractPresent = decoder("bool", in_msg[msg_counter()])
    # needs to be TRUE
    if (deltaNeutralContractPresent) {
      contract$deltaNeutralContract = DeltaNeutralContract()
      contract$deltaNeutralcontract$conId = decoder("integer", in_msg[msg_counter()])
      contract$deltaNeutralcontract$delta = decoder("numeric", in_msg[msg_counter()])
      contract$deltaNeutralcontract$price = decoder("numeric", in_msg[msg_counter()])
    }
  }

  # algostrategy fields
  if (version >= 21){
    order$algoStrategy = in_msg[msg_counter()]
    if (nchar(order$algoStrategy) > 0){
      algoParamsCount = decoder("integer", in_msg[msg_counter()])
      if (algoParamsCount > 0){
        order$algoParams = list()
        for (i in seq_len(algoParamsCount)){
          tagValue = TagValue()
          tagValue.tag = in_msg[msg_counter()]
          tagValue.value = in_msg[msg_counter()]
          order$algoParams[i] <- tagValue
        }
      }
    }
  }
  # end algostrategy fields

  if (version >= 33){
    order$solicited = decoder("bool", in_msg[msg_counter()])
  }

  order$whatIf = decoder("bool", in_msg[msg_counter()])

  #  OrderState fields
  orderState = OrderState()

  orderState.status = in_msg[msg_counter()] # ver 16 field
  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_WHAT_IF_EXT_FIELDS){
    orderState.initMarginBefore = in_msg[msg_counter()]
    orderState.maintMarginBefore = in_msg[msg_counter()]
    orderState.equityWithLoanBefore = in_msg[msg_counter()]
    orderState.initMarginChange = in_msg[msg_counter()]
    orderState.maintMarginChange = in_msg[msg_counter()]
    orderState.equityWithLoanChange = in_msg[msg_counter()]
  }

  orderState.initMarginAfter = in_msg[msg_counter()]
  orderState.maintMarginAfter = in_msg[msg_counter()]
  orderState.equityWithLoanAfter = in_msg[msg_counter()]

  orderState.commission = decoder("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  orderState.minCommission = decode("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  orderState.maxCommission = decode("numeric", in_msg[msg_counter()]) # SHOW_UNSET
  orderState.commissionCurrency = in_msg[msg_counter()]
  orderState.warningText = in_msg[msg_counter()]
  # end OrderState fields

  if (version >= 34){
    order$randomizeSize = decoder("bool", in_msg[msg_counter()])
    order$randomizePrice = decoder("bool", in_msg[msg_counter()])
  }

  # pegged to benchmark fields
  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_PEGGED_TO_BENCHMARK){
    if (order$orderType == "PEG BENCH") {
      order$referenceContractId = decoder("integer", in_msg[msg_counter()])
      order$isPeggedChangeAmountDecrease = decoder("bool", in_msg[msg_counter()])
      order$peggedChangeAmount = decoder("numeric", in_msg[msg_counter()])
      order$referenceChangeAmount = decoder("numeric", in_msg[msg_counter()])
      order$referenceExchangeId = in_msg[msg_counter()]
    }

    # TODO: figure out OrderCondition
    conditionsSize = decoder("integer", in_msg[msg_counter()])
    if (conditionsSize > 0){
      order$conditions = list()
      for (i in seq_len(conditionsSize)){
        conditionType = decoder("integer", in_msg[msg_counter()])
        condition = order_condition.Create(conditionType)
        condition.decode(in_msg[msg_counter()])
        order$conditions[i] = condition
      }
    }

    order$conditionsIgnoreRth = decoder("bool", in_msg[msg_counter()])
    order$conditionsCancelOrder = decoder("bool", in_msg[msg_counter()])

    order$adjustedOrderType = in_msg[msg_counter()]
    order$triggerPrice = decoderr("numeric", in_msg[msg_counter()])
    order$trailStopPrice = decoder("numeric", in_msg[msg_counter()])
    order$lmtPriceOffset = decoder("numeric", in_msg[msg_counter()])
    order$adjustedStopPrice = decoder("numeric", in_msg[msg_counter()])
    order$adjustedStopLimitPrice = decoder("numeric", in_msg[msg_counter()])
    order$adjustedTrailingAmount = decoder("numeric", in_msg[msg_counter()])
    order$adjustableTrailingUnit = decoder("integer", in_msg[msg_counter()])
  }
  # end pegged to benchmark fields


  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_SOFT_DOLLAR_TIER){
    name = in_msg[msg_counter()]
    value = in_msg[msg_counter()]
    displayName = in_msg[msg_counter()]
    order$softDollarTier = SoftDollarTier(name, value, displayName)}

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_CASH_QTY){
    order$cashQty = decoder("numeric", in_msg[msg_counter()])}

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_AUTO_PRICE_FOR_HEDGE){
    order$dontUseAutoPriceForHedge = decoder("bool", in_msg[msg_counter()])}

  if( ib_con$server_version >= .server_version$MIN_SERVER_VER_ORDER_CONTAINER){
    order$isOmsContainer = decoder("bool", in_msg[msg_counter()])}

  if (ib_con$server_version >= .server_version$MIN_SERVER_VER_D_PEG_ORDERS){
    order$discretionaryUpToLimitPrice = decoder("bool", in_msg[msg_counter()])}

  # TODO: call to openOrder object in ewrapper.
  openOrder(order$orderId, contract, order, orderState)

}







