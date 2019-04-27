#' @keywords internal
Order = function(
    # order identifier
    orderId  = 0,
    clientId = 0,
    permId   = 0,

    # main order fields
    action = "",
    totalQuantity = 0,
    orderType = "",
    lmtPrice  = numeric(length = 0),
    auxPrice  = numeric(length = 0),

    # extended order fields
    tif = "",                # "Time in Force" - DAY, GTC, etc.
    activeStartTime = "",   # for GTC orders
    activeStopTime = "",    # for GTC orders
    ocaGroup = "",          # one cancels all group name
    ocaType        = 0,     # 1 = CANCEL_WITH_BLOCK, 2 = REDUCE_WITH_BLOCK, 3 = REDUCE_NON_BLOCK
    orderRef       = "",
    transmit       = TRUE,  # if false, order will be created but not transmited
    parentId       = 0,     # Parent order Id, to associate Auto STP or TRAIL orders with the original order.
    blockOrder     = FALSE,
    sweepToFill    = FALSE,
    displaySize    = 0,
    triggerMethod  = 0,    # 0=Default, 1=Double_Bid_Ask, 2=Last, 3=Double_Last, 4=Bid_Ask, 7=Last_or_Bid_Ask, 8=Mid-point
    outsideRth     = FALSE,
    hidden         = FALSE,
    goodAfterTime       = "",   # Format: 20060505 08:00:00 {time zone}
    goodTillDate        = "",  # Format: 20060505 08:00:00 {time zone}
    rule80A             = "",   # Individual = 'I', Agency = 'A', AgentOtherMember = 'W', IndividualPTIA = 'J', AgencyPTIA = 'U', AgentOtherMemberPTIA = 'M', IndividualPT = 'K', AgencyPT = 'Y', AgentOtherMemberPT = 'N'
    allOrNone      = FALSE,
    minQty         = integer(length = 0),  #type: int
    percentOffset  = numeric(length = 0), # type: float; REL orders only
    overridePercentageConstraints = FALSE,
    trailStopPrice = numeric(length = 0),  # type: float
    trailingPercent = numeric(length = 0), # type: float; TRAILLIMIT orders only

    # financial advisors only
    faGroup              = "",
    faProfile            = "",
    faMethod             = "",
    faPercentage         = "",

    # institutional (ie non-cleared) only
    designatedLocation = "", #used only when shortSaleSlot=2
    openClose     = "O",    # O=Open, C=Close
    origin        = 0,  # 0=Customer, 1=Firm
    shortSaleSlot = 0,    # type: int; 1 if you hold the shares, 2 if they will be delivered from elsewhere.  Only for Action=SSHORT
    exemptCode    = -1,

    # SMART routing only
    discretionaryAmt = 0,
    eTradeOnly   = TRUE,
    firmQuoteOnly    = TRUE,
    nbboPriceCap = numeric(length = 0),
    optOutSmartRouting = FALSE,

    # BOX exchange orders only
    auctionStrategy = 0, # 1=AUCTION_MATCH, 2=AUCTION_IMPROVEMENT, 3=AUCTION_TRANSPARENT
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

    # HEDGE ORDERS
    hedgeType             = "", # 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
    hedgeParam            = "", # 'beta=X' value for beta hedge, 'ratio=Y' for pair hedge

    # Clearing info
    account               = "", # IB account
    settlingFirm          = "",
    clearingAccount       = "",   #True beneficiary of the order
    clearingIntent        = "", # "" (Default), "IB", "Away", "PTA" (PostTrade)

    # ALGO ORDERS ONLY
    algoStrategy          = "",

    algoParams            = NULL,    #TagValueList
    smartComboRoutingParams = NULL,  #TagValueList

    algoId = "",


    # What-if
    whatIf = FALSE,

    # Not Held
    notHeld = FALSE,
    solicited = FALSE,

    # models
    modelCode = "",

    # order combo legs
    orderComboLegs = NULL,  # OrderComboLegListSPtr

    orderMiscOptions = NULL,  # TagValueList

    # VER PEG2BENCH fields:
    referenceContractId = 0,
    peggedChangeAmount = 0.0,
    isPeggedChangeAmountDecrease = FALSE,
    referenceChangeAmount = 0.0,
    referenceExchangeId = "",
    adjustedOrderType = "",

    triggerPrice = numeric(length = 0),
    adjustedStopPrice = numeric(length = 0),
    adjustedStopLimitPrice = numeric(length = 0),
    adjustedTrailingAmount = numeric(length = 0),
    adjustableTrailingUnit = 0,
    lmtPriceOffset = numeric(length = 0),

    conditions = NULL,  # std::vector<std::shared_ptr<OrderCondition>>
    conditionsCancelOrder = FALSE,
    conditionsIgnoreRth = FALSE,

    # ext operator
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
      action = action,
      totalQuantity = totalQuantity,
      orderType = orderType,
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

      # financial advisors only
      faGroup              = faGroup,
      faProfile            = faProfile,
      faMethod             = faMethod,
      faPercentage         = faPercentage,

      # institutional (ie non-cleared) only
      designatedLocation = designatedLocation, #used only when shortSaleSlot=2
      openClose     = openClose,    # O=Open, C=Close
      origin        = origin,  # 0=Customer, 1=Firm
      shortSaleSlot = shortSaleSlot,    # type: int; 1 if you hold the shares, 2 if they will be delivered from elsewhere.  Only for Action=SSHORT
      exemptCode    = exemptCode,

      # SMART routing only
      discretionaryAmt = discretionaryAmt,
      eTradeOnly   = eTradeOnly,
      firmQuoteOnly    = firmQuoteOnly,
      nbboPriceCap = nbboPriceCap,
      optOutSmartRouting = optOutSmartRouting,

      # BOX exchange orders only
      auctionStrategy = auctionStrategy, # 1=AUCTION_MATCH, 2=AUCTION_IMPROVEMENT, 3=AUCTION_TRANSPARENT
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

      # HEDGE ORDERS
      hedgeType             = hedgeType, # 'D' - delta, 'B' - beta, 'F' - FX, 'P' - pair
      hedgeParam            = hedgeParam, # 'beta=X' value for beta hedge, 'ratio=Y' for pair hedge

      # Clearing info
      account               = account, # IB account
      settlingFirm          = settlingFirm,
      clearingAccount       = clearingAccount,   #True beneficiary of the order
      clearingIntent        = clearingIntent, # "" (Default), "IB", "Away", "PTA" (PostTrade)

      # ALGO ORDERS ONLY
      algoStrategy          = algoStrategy,

      algoParams            = algoParams,    #TagValueList
      smartComboRoutingParams = smartComboRoutingParams,  #TagValueList

      algoId = algoId,


      # What-if
      whatIf = whatIf,

      # Not Held
      notHeld = notHeld,
      solicited = solicited,

      # models
      modelCode = modelCode,

      # order combo legs
      orderComboLegs = orderComboLegs,  # OrderComboLegListSPtr

      orderMiscOptions = orderMiscOptions,  # TagValueList

      # VER PEG2BENCH fields:
      referenceContractId = referenceContractId,
      peggedChangeAmount = peggedChangeAmount,
      isPeggedChangeAmountDecrease = isPeggedChangeAmountDecrease,
      referenceChangeAmount = referenceChangeAmount,
      referenceExchangeId = referenceExchangeId,
      adjustedOrderType = adjustedOrderType,

      triggerPrice = triggerPrice,
      adjustedStopPrice = adjustedStopPrice,
      adjustedStopLimitPrice = adjustedStopLimitPrice,
      adjustedTrailingAmount = adjustedTrailingAmount,
      adjustableTrailingUnit = adjustableTrailingUnit,
      lmtPriceOffset = lmtPriceOffset,

      conditions = conditions,  # std::vector<std::shared_ptr<OrderCondition>>
      conditionsCancelOrder = conditionsCancelOrder,
      conditionsIgnoreRth = conditionsIgnoreRth,

      # ext operator
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

#' @keywords internal
OrderStatus <- function(orderId = 0L,
                        status = '',
                        filled = 0L,
                        remaining = 0L,
                        avgFillPrice = 0.0,
                        permId = 0L,
                        parentId = 0L,
                        lastFillPrice = 0.0,
                        clientId = 0L,
                        whyHeld = '',
                        mktCapPrice = 0.0
                        #lastLiquidity = 0L
                        ){
  structure(
    list(
      orderId = orderId,
      status = status,
      filled = filled,
      remaining = remaining,
      avgFillPrice = avgFillPrice,
      permId = permId,
      parentId = parentId,
      lastFillPrice = lastFillPrice,
      clientId = clientId,
      whyHeld = whyHeld,
      mktCapPrice = mktCapPrice
      # lastLiquidity = lastLiquidity
    )
  )

}




# TODO: print method for class Order
# xx <- Order()
# data.frame(unlist(xx), stringsAsFactors = FALSE)





