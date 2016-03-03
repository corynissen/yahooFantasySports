#' Get a list of players in your league
#'
#' Get a list of players in your league
#' @param token The token returned from auth.R. Must have class "Token"
#' @param league_id The league ID for your Yahoo fantasy league as a character string
#' @keywords players
#' @export
#' @return A token to use when making requests from the Yahoo API
#' @examples
#' \dontrun{
#' player_df <- players(token, league_id)
#' }
#' players
players <- function(token, league_id){
  if(!"Token" %in% class(token)){stop('token must have class "Token"')}
  if(!is.character(league_id)){stop("league_id must be a character string")}
  
  url <- "http://fantasysports.yahooapis.com/fantasy/v2/"
  url <- paste0(url, league_id, "//players?format=json")
  
  https://query.yahooapis.com/v1/yql?q=select%20*%20from%20fantasysports.games%20where%20game_key%3D%22238%22&diagnostics=true
  
  url <- 'https://query.yahooapis.com/v1/yql?q=select * from fantasysports.players where league_key="387563"&display_position="WR"&format=json'
  url <- paste0(url, 
  url <- paste0(url, game_key, '"&diagnostics=true&format=json')
  
  url1 <- 'http://fantasysports.yahooapis.com/fantasy/v2/league/348.l.387563/players?format=json&start=25'
  url2 <- 'http://fantasysports.yahooapis.com/fantasy/v2/league/348.l.387563/players?format=json'
  
  p <- httr::GET(utils::URLencode(url1), httr::config(token=token))
  a1 <- httr::content(p)
  
  p <- httr::GET(utils::URLencode(url2), httr::config(token=token))
  a2 <- httr::content(p)
  
  
  
  l1 <- length(a1$fantasy_content$league[[2]]$players) - 1
  l2 <- length(a2$fantasy_content$league[[2]]$players) - 1

  t1 <- sapply(1:l1, function (x)a1$fantasy_content$league[[2]]$players[[x]]$player[[1]][[3]]$name$full)
  t2 <- sapply(1:l2, function (x)a2$fantasy_content$league[[2]]$players[[x]]$player[[1]][[3]]$name$full)
}
