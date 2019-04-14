#' Establish a connection to IB trader workstation or to the IB Gateway
#'
#' @param clientId The client ID used in the API connection. Default value is 1.
#' @param host Default connection is 127.0.0.1 for the trader workstation or the IB Gateway
#' @param port Default connection is to the paper trading account on port 7497.
#' Live connection to TWS is on port 7496. For the IB gateway the live port is 4001
#' and the paper trading port is 4002.
#' @param timeout Time to establish a connection in seconds. Default is 5.
#' @param blocking Create a blocking connection. Defaults to TRUE on Windows, FALSE otherwise.
#'
#' @return Returns an ib_con object.
#' @export
#'
#' @examples
#' \dontrun{
#' ib_con <- ib_connect()
#' ib_disconnect(ib_con)
#' }
ib_connect <-
  function (clientId = 1L, host = "127.0.0.1", port = 7497,
            timeout = 5, blocking = .Platform$OS.type == "windows") {

    # TODO: tests

    # ibapi needs an api trigger
    start_api <- function(ib_con, clientId) {

      # connectOptions for now empty
      # + PACEAPI flag under review
      optCapab <- ""
      VERSION <- "2"

      out_msg <- c(.outgoing_msg_id$START_API,
                   VERSION,
                   make_field(clientId))
      if (ib_con$server_version > .server_version$MIN_SERVER_VER_OPTIONAL_CAPABILITIES) {
        out_msg <- c(out_msg, make_field(optCapab))
      }

      send_message(out_msg, ib_con)
    }



    if (is.null(getOption("digits.secs")))
      options(digits.secs = 6)

    # open connection ----------
    start.time <- Sys.time()
    sock_con <- socketConnection(host = host,
                                 port = port,
                                 open = "ab",
                                 blocking = blocking)
    on.exit(close(sock_con))

    if (!isOpen(sock_con)) {
      close(sock_con)
      stop(glue("couldn't connect to TWS on port: {port}"), call. = FALSE)
    }

    # send client version using v100Plus protocol
    v100prefix = "API"
    if (.client_version$MIN_CLIENT_VER < .client_version$MAX_CLIENT_VER) {
      v100msg <- sprintf("v%s..%s", .client_version$MIN_CLIENT_VER, .client_version$MAX_CLIENT_VER)
    }

    # use different message length calculation. No need to send a terminator field
    msg_length <- nchar(v100msg)
    writeBin(v100prefix, sock_con)
    writeBin(msg_length, sock_con, endian = "big")
    # prevent sending of terminator otherwise connection will fail
    writeChar(v100msg, sock_con, eos = NULL)

    SERVER_VERSION <- NEXT_VALID_ID <- CONNECTION_TIME <- NULL
    # Server Version and connection time
    while (TRUE) {
      if (!is.null(CONNECTION_TIME))
        break
      if (!socketSelect(list(sock_con), FALSE, 0.1))
        next

      parsed_message <- parse_incoming_message(sock_con)

      if (is.null(SERVER_VERSION)) {
        SERVER_VERSION <- parsed_message[1]
        CONNECTION_TIME <- parsed_message[2]
        next
      }

      if (Sys.time()-start.time > timeout) {
        close(sock_con)
        stop('ib connection timed-out')
      }
    }

    on.exit() # connection succeeded

    ib_con <- new.env()
    ib_con$con <- sock_con
    ib_con$clientId <- clientId
    ib_con$port <- port
    ib_con$server_version <- as.integer(SERVER_VERSION)
    ib_con$connected_at <- CONNECTION_TIME
    ib_con$connected <- is_ib_connection_open(ib_con)
    class(ib_con) <- c("ib_con", "environment")

    # ib needs an API call message
    start_api(ib_con, clientId)

    # pause a bit
    Sys.sleep(1)

    # read and process connection messages ----------

    while (TRUE) {
      if (socketSelect(list(sock_con), FALSE, 1)) {
        in_msg <- parse_incoming_message(sock_con)
        msgId <- in_msg[1]

        if (msgId == .incoming_msg_id$MANAGED_ACCTS) {
          ib_con$accounts <- ProcessMsg(in_msg, ib_con)
        } else
          if (msgId == .incoming_msg_id$NEXT_VALID_ID) {
            ib_con$nextValidId <- ProcessMsg(in_msg, ib_con)
          } else
            # print Market data farm messages
            if (msgId == .incoming_msg_id$ERR_MSG) {
              ProcessMsg(in_msg, ib_con)
            }
      } else
        break
    }

    if (length(ib_con$accounts) > 1) {
      print(glue('Your TWS user name handles {length(ib_con$accounts)} accounts.',
                 ' Read the documentation with the req_managed_accounts function'))
    }
    print(glue("You are connected to account: {ib_con$accounts}"))

    return(ib_con)
  }


#' @rdname ib_connect
#' @export
is_ib_connection <- function(x) {
  inherits(x, "ib_con")
}


#' @rdname ib_connect
#' @export
ib_disconnect <- function(ib_con) {
  if (!is_ib_connection(ib_con)) {
    stop("Not an ib connection", call. = FALSE)
  }

  if (is_ib_connection_open(ib_con)) {
    ib_con$connected <- FALSE
    ib_con$connected_at <- NULL
    ib_con$server_version <- 0L
    ib_con$clientId <- -1L
    ib_con$nextValidId <- 0L
    ib_con$accounts <- NULL
    close(ib_con$con)
    glue("The ib connection is now closed")
  } else {
    glue("The ib connection is already closed.")
  }
}

#' @rdname ib_connect
#' @export
is_ib_connected <- function(ib_con){
  if (!is_ib_connection(ib_con)) {
    stop("Not an ib connection", call. = FALSE)
  }

  if (is_ib_connection_open(ib_con)){
    glue("Connection is open and was opened at: {ib_con$connected_at}.")
  } else {
    glue("Connection is closed.")
  }
}

#' @keywords internal
is_ib_connection_open <- function(ib_con) {
  if (inherits(try(isOpen(ib_con$con), silent=TRUE), 'try-error')) {
    FALSE
  } else TRUE
}

