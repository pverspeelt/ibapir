#' @keywords internal
Order = function(
    # order identifier
    orderId  = 0,
    clientId = 0,
    permId   = 0,

    # main order fields
    totalQuantity = 0,
    lmtPrice  = numeric(length = 0),
    auxPrice  = numeric(length = 0),

    # extended order fields
    activeStartTime = "",
    activeStopTime = "",
    ocaType    = 0,
    transmit   = TRUE,
    parentId   = 0,
    blockOrder = FALSE,
    sweepToFill    = FALSE,
    displaySize    = 0,
    triggerMethod  = 0,
    outsideRth = FALSE,
    hidden     = FALSE,
    allOrNone  = FALSE,
    minQty     = integer(length = 0),
    percentOffset  = numeric(length = 0),
    overridePercentageConstraints = FALSE,
    trailStopPrice = numeric(length = 0),
    trailingPercent = numeric(length = 0),

    # institutional (ie non-cleared) only
    openClose = "O",
    origin    = 0,
    shortSaleSlot = 0,
    exemptCode    = -1,

    # SMART routing only
    discretionaryAmt = 0,
    eTradeOnly   = TRUE,
    firmQuoteOnly    = TRUE,
    nbboPriceCap = numeric(length = 0),
    optOutSmartRouting = FALSE,

    # BOX exchange orders only
    auctionStrategy = 0,
    startingPrice   = numeric(length = 0),
    stockRefPrice   = numeric(length = 0),
    delta       = numeric(length = 0),

    # pegged to stock and VOL orders only
    stockRangeLower = numeric(length = 0),
    stockRangeUpper = numeric(length = 0),

    randomizePrice = FALSE,
    randomizeSize = FALSE,

    # VOLATILITY ORDERS ONLY
    volatility        = numeric(length = 0),
    volatilityType    = integer(length = 0),     # 1=daily, 2=annual
    deltaNeutralOrderType = "",
    deltaNeutralAuxPrice  = numeric(length = 0),
    deltaNeutralConId = 0,
    deltaNeutralSettlingFirm = "",
    deltaNeutralClearingAccount = "",
    deltaNeutralClearingIntent = "",
    deltaNeutralOpenClose = "",
    deltaNeutralShortSale = FALSE,
    deltaNeutralShortSaleSlot = 0,
    deltaNeutralDesignatedLocation = "",
    continuousUpdate  = FALSE,
    referencePriceType    = integer(length = 0), # 1=Average, 2 = BidOrAsk

    # COMBO ORDERS ONLY
    basisPoints = numeric(length = 0),  # EFP orders only
    basisPointsType = integer(length = 0), # EFP orders only

    # SCALE ORDERS ONLY
    scaleInitLevelSize  = integer(length = 0),
    scaleSubsLevelSize  = integer(length = 0),
    scalePriceIncrement = numeric(length = 0),
    scalePriceAdjustValue = numeric(length = 0),
    scalePriceAdjustInterval = integer(length = 0),
    scaleProfitOffset = numeric(length = 0),
    scaleAutoReset = FALSE,
    scaleInitPosition = integer(length = 0),
    scaleInitFillQty = integer(length = 0),
    scaleRandomPercent = FALSE,
    scaleTable = "",

    # What-if
    whatIf = FALSE,

    # Not Held
    notHeld = FALSE,
    solicited = FALSE,

    triggerPrice = numeric(length = 0),
    adjustedStopPrice = numeric(length = 0),
    adjustedStopLimitPrice = numeric(length = 0),
    adjustedTrailingAmount = numeric(length = 0),
    lmtPriceOffset = numeric(length = 0),
    extOperator = "",

    # native cash quantity
    cashQty = numeric(length = 0),

    mifid2DecisionMaker = "",
    mifid2DecisionAlgo = "",
    mifid2ExecutionTrader = "",
    mifid2ExecutionAlgo = "",

    # don't use auto price for hedge
		dontUseAutoPriceForHedge = FALSE,

    isOmsContainer = FALSE,
    discretionaryUpToLimitPrice = FALSE

    # new values available in version 9.75
) {

  structure(
    list(
      orderId  = orderId,
      clientId = clientId,
      permId   = permId,

      # main order fields
      totalQuantity = totalQuantity,
      lmtPrice  = lmtPrice,
      auxPrice  = auxPrice,

      # extended order fields
      activeStartTime = activeStartTime,
      activeStopTime = activeStopTime,
      ocaType    = ocaType,
      transmit   = transmit,
      parentId   = parentId,
      blockOrder = blockOrder,
      sweepToFill    = sweepToFill,
      displaySize    = displaySize,
      triggerMethod  = triggerMethod,
      outsideRth = outsideRth,
      hidden     = hidden,
      allOrNone  = allOrNone,
      minQty     = minQty,
      percentOffset  = percentOffset,
      overridePercentageConstraints = overridePercentageConstraints,
      trailStopPrice = trailStopPrice,
      trailingPercent = trailingPercent,

      # institutional (ie non-cleared) only
      openClose = openClose,
      origin    = origin,
      shortSaleSlot = shortSaleSlot,
      exemptCode    = exemptCode,

      # SMART routing only
      discretionaryAmt = discretionaryAmt,
      eTradeOnly   = eTradeOnly,
      firmQuoteOnly    = firmQuoteOnly,
      nbboPriceCap = nbboPriceCap,
      optOutSmartRouting = optOutSmartRouting,

      # BOX exchange orders only
      auctionStrategy = auctionStrategy,
      startingPrice   = startingPrice,
      stockRefPrice   = stockRefPrice,
      delta       = delta,

      # pegged to stock and VOL orders only
      stockRangeLower = stockRangeLower,
      stockRangeUpper = stockRangeUpper,

      randomizePrice = randomizePrice,
      randomizeSize = randomizeSize,

      # VOLATILITY ORDERS ONLY
      volatility        = volatility,
      volatilityType    = volatilityType,     # 1=daily, 2=annual
      deltaNeutralOrderType = deltaNeutralOrderType,
      deltaNeutralAuxPrice  = deltaNeutralAuxPrice,
      deltaNeutralConId = deltaNeutralConId,
      deltaNeutralSettlingFirm = deltaNeutralSettlingFirm,
      deltaNeutralClearingAccount = deltaNeutralClearingAccount,
      deltaNeutralClearingIntent = deltaNeutralClearingIntent,
      deltaNeutralOpenClose = deltaNeutralOpenClose,
      deltaNeutralShortSale = deltaNeutralShortSale,
      deltaNeutralShortSaleSlot = deltaNeutralShortSaleSlot,
      deltaNeutralDesignatedLocation = deltaNeutralDesignatedLocation,
      continuousUpdate  = continuousUpdate,
      referencePriceType    = referencePriceType, # 1=Average, 2 = BidOrAsk

      # COMBO ORDERS ONLY
      basisPoints = basisPoints,  # EFP orders only
      basisPointsType = basisPointsType, # EFP orders only

      # SCALE ORDERS ONLY
      scaleInitLevelSize  = scaleInitLevelSize,
      scaleSubsLevelSize  = scaleSubsLevelSize,
      scalePriceIncrement = scalePriceIncrement,
      scalePriceAdjustValue = scalePriceAdjustValue,
      scalePriceAdjustInterval = scalePriceAdjustInterval,
      scaleProfitOffset = scaleProfitOffset,
      scaleAutoReset = scaleAutoReset,
      scaleInitPosition = scaleInitPosition,
      scaleInitFillQty = scaleInitFillQty,
      scaleRandomPercent = scaleRandomPercent,
      scaleTable = scaleTable,

      # What-if
      whatIf = whatIf,

      # Not Held
      notHeld = notHeld,
      solicited = solicited,

      triggerPrice = triggerPrice,
      adjustedStopPrice = adjustedStopPrice,
      adjustedStopLimitPrice = adjustedStopLimitPrice,
      adjustedTrailingAmount = adjustedTrailingAmount,
      lmtPriceOffset = lmtPriceOffset,
      extOperator = extOperator,

      # native cash quantity
      cashQty = cashQty,

      mifid2DecisionMaker = mifid2DecisionMaker,
      mifid2DecisionAlgo = mifid2DecisionAlgo,
      mifid2ExecutionTrader = mifid2ExecutionTrader,
      mifid2ExecutionAlgo = mifid2ExecutionAlgo,

      # don't use auto price for hedge
      dontUseAutoPriceForHedge = dontUseAutoPriceForHedge,

      isOmsContainer = isOmsContainer,
      discretionaryUpToLimitPrice = discretionaryUpToLimitPrice
    ),
    class = "Order"
  )

}

# TODO: print method for class Order
# xx <- Order()
# data.frame(unlist(xx), stringsAsFactors = FALSE)
