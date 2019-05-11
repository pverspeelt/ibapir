#' Request news providers
#'
#' Requests news providers which the user has subscribed to.
#'
#' @param ib_con A valid ib connection.
#'
#' @return A data.frame with an overview of the news providers
#' @export
#'
#' @examples
#' Example to follow
reqNewsProviders <- function(ib_con){

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_REQ_NEWS_PROVIDERS) {
    stop(glue("Current server version {ib_con$server_version} does not support news providers request."),
         call. = FALSE)
  }

  out_msg = make_field(.outgoing_msg_id$REQ_NEWS_PROVIDERS)

  send_message(out_msg, ib_con)

  # receive messages
  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$NEWS_PROVIDERS) {
        newsProviders <- ProcessMsg(in_msg, ib_con)
      } else
      if (msgId == .incoming_msg_id$ERR_MSG) {
        ProcessMsg(in_msg, ib_con)
      }
    } else
      break
  }
  return(newsProviders)
}


#' @keywords internal
processNewsProvidersMsg <- function(in_msg, msg_counter) {

  nNewsProviders = decoder("integer", in_msg[msg_counter()])

  if (nNewsProviders > 0) {
    newsProviders = data.frame(code = character(length = nNewsProviders),
                               name = character(length = nNewsProviders),
                               stringsAsFactors = FALSE)

    for (i in seq_len(nNewsProviders)){
      newsProviders$code[i] = in_msg[msg_counter()]
      newsProviders$name[i] = in_msg[msg_counter()]
    }
  }
  return(newsProviders)
}


#' Request News Article Body
#'
#' Requests news article body given articleId
#'
#' @param reqId id to specify the request. Should be a scalar integer.
#' @param providerCode Short code indicating news provider, e.g. FLY
#' @param articleId id of the specific article
#' @param ib_con A valid ib connection.
#'
#' @return a data.frame containing the article type and article text
#' @export
#'
#' @examples
#' Example to follow
reqNewsArticle <- function(reqId,
                           providerCode,
                           articleId,
                           ib_con){

  if (ib_con$server_version < .server_version$MIN_SERVER_VER_REQ_NEWS_ARTICLE){
    stop(glue("Current server version {ib_con$server_version} does not support news article request."),
         call. = FALSE)
  }

  out_msg <- c(make_field(.outgoing_msg_id$REQ_NEWS_ARTICLE),
               make_field(reqId),
               make_field(providerCode),
               make_field(articleId))

  # send newsArticleOptions parameter
  if (ib_con$server_version  >= .server_version$MIN_SERVER_VER_NEWS_QUERY_ORIGINS) {
    newsArticleOptionsStr = ""
  } # reserved for internal use. Should be defined as null.

  out_msg = c(out_msg,
              make_field(newsArticleOptionsStr))

  send_message(out_msg, ib_con)

  # receive messages
  while (TRUE) {
    if (socketSelect(list(ib_con$con), FALSE, 0.1)) {
      in_msg <- parse_incoming_message(ib_con$con)
      msgId <- in_msg[1]

      if (msgId == .incoming_msg_id$NEWS_ARTICLE) {
        article <- ProcessMsg(in_msg, ib_con)
      } else
        if (msgId == .incoming_msg_id$ERR_MSG) {
          ProcessMsg(in_msg, ib_con)
        }
    } else
      break
  }
  return(article)
}

#' @keywords internal
processNewsArticleMsg <- function(in_msg, msg_counter){

  reqId = decoder("integer", in_msg[msg_counter()])
  articleType = decoder("integer", in_msg[msg_counter()])
  articleText = in_msg[msg_counter()]

  article <- data.frame(articleType, articleText)

}

