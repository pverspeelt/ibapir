#' @keywords internal
OrderState <- function(status = "",
                       initMarginBefore = "",
                       maintMarginBefore = "",
                       equityWithLoanBefore = "",
                       initMarginChange = "",
                       maintMarginChange = "",
                       equityWithLoanChange = "",
                       initMarginAfter = "",
                       maintMarginAfter = "",
                       equityWithLoanAfter = "",
                       commission = numeric(length = 0),
                       minCommission = numeric(length = 0),
                       maxCommission =numeric(length = 0),
                       commissionCurrency = "",
                       warningText = "",
                       completedTime = "",
                       completedStatus = "") {

  structure(
    list(
      status = status,
      initMarginBefore = initMarginBefore,
      maintMarginBefore = maintMarginBefore,
      equityWithLoanBefore = equityWithLoanBefore,
      initMarginChange = initMarginChange,
      maintMarginChange = maintMarginChange,
      equityWithLoanChange = equityWithLoanChange,
      initMarginAfter = initMarginAfter,
      maintMarginAfter = maintMarginAfter,
      equityWithLoanAfter = equityWithLoanAfter,
      commission = commission,
      minCommission = minCommission,
      maxCommission = maxCommission,
      commissionCurrency = commissionCurrency,
      warningText = warningText,
      completedTime = completedTime,
      completedStatus = completedStatus
      ),
    class = "OrderState"
    )
}


