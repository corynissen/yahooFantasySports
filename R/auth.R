#' Authenticate with Yahoo API
#'
#' Use your credentials to authenticate with the Yahoo API. This will open a browser window to authenticate with Yahoo.
#' @param key Consumer Key from Yahoo Developer Network
#' @param secret Consumer Secret from Yahoo Developer Network
#' @keywords authenticate
#' @export
#' @return A token to use when making requests from the Yahoo API
#' @examples
#' \dontrun{
#' token <- auth(key="my_key", secret="my_secret")
#' }
#' auth
auth <- function(key, secret){
  if(!is.character(key)){stop("key must be a character string")}
  if(!is.character(secret)){stop("secret must be a character string")}
  
  httr::oauth_endpoints("yahoo")
  myapp <- httr::oauth_app("yahoo", key = key, secret = secret)
  token <- httr::oauth1.0_token(httr::oauth_endpoints("yahoo"), myapp, 
                                cache=FALSE)
  return(token)
}
