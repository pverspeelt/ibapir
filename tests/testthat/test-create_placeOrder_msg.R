test_that("create_placeOrder_msg error messages", {
  x <- LimitOrder("BUY", totalQuantity <- 100, lmtPrice <- 25)
  ibm <- Stock("IBM")

  my_con <- list()
  my_con$server_version <- 102
  x$modelCode <- "test_model"
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support model code parameter."))

  my_con$server_version <- 104
  x$extOperator <- "operator"
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support ext operator parameter."))

  my_con$server_version <- 105
  x$softDollarTier$name <- "test_value"
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support soft dollar tier."))

  my_con$server_version <- 110
  x$cashQty <- 5
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support cash quantity parameter."))

  my_con$server_version <- 137
  x$mifid2DecisionMaker <- "test_value"
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support MIFID II decision maker parameters."))

  my_con$server_version <- 138
  x$mifid2ExecutionTrader <- "test_value"
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support MIFID II execution parameters."))

  my_con$server_version <- 140
  x$dontUseAutoPriceForHedge <- TRUE
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support dontUseAutoPriceForHedge parameter."))

  my_con$server_version <- 144
  x$isOmsContainer <- TRUE
  expect_error(ibapir:::create_placeOrder_msg(1L, ibm, x, my_con),
               glue::glue("Current server version {my_con$server_version} does not support oms container parameter."))

})


test_that("create_placeOrder_msg message lengths", {
  x <- LimitOrder("BUY", totalQuantity <- 100, lmtPrice <- 25)
  ibm <- Stock("IBM")
  my_con <- list()

  #TODO: add other server versions
  #TODO: add counts for for loops in msg

  # current version
  my_con$server_version <- 148
  msg <- ibapir:::create_placeOrder_msg(1L, ibm, x, my_con)
  expect_length(msg, 109)
})


test_that("create_placeOrder_msg version is gone", {
  x <- LimitOrder("BUY", totalQuantity <- 100, lmtPrice <- 25)
  ibm <- Stock("IBM")
  my_con <- list()

  # current version
  my_con$server_version <- .client_version$MAX_CLIENT_VER
  msg <- ibapir:::create_placeOrder_msg(1L, ibm, x, my_con)
  # from version 145 the VERSION is not used anymore. Check to see if this doesn't reappear.
  expect_equal(as.integer(msg[2]), 1L)
})
