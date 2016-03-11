#' Get a list of players in your league
#'
#' Get a list of players in your league
#' @param token The token returned from auth.R. Must have class "Token"
#' @param league_id The league ID for your Yahoo fantasy league as a character string
#' @param n The number of players to request
#' @keywords players
#' @export
#' @return A token to use when making requests from the Yahoo API
#' @examples
#' \dontrun{
#' player_df <- players(token, league_id)
#' }
#' players
players <- function(token, league_id, n=25){
  if(!"Token" %in% class(token)){stop('token must have class "Token"')}
  if(!is.character(league_id)){stop("league_id must be a character string")}
  if(!is.numeric(n)){stop("n must be numeric")}
  
  url <- 'http://fantasysports.yahooapis.com/fantasy/v2/league/348.l.387563/players?format=json&start='
  
  p <- httr::GET(utils::URLencode(url1), httr::config(token=token))
  a1 <- httr::content(p)
  
  p <- httr::GET(utils::URLencode(url2), httr::config(token=token))
  a2 <- httr::content(p)
  
  a <- sapply(a1$fantasy_content$league[[2]]$players, function(x)unlist(x))
  cols_to_keep <- c("player.name.ascii_first", "player.name.ascii_last",
                     "player.player_key", "player.player_id", 
                     "player.uniform_number", "player.display_position",
                     "player.position_type", "player.editorial_team_abbr",
                     "player.editorial_team_full_name")
  tmp <- sapply(a, function(x)x[cols_to_keep])
  tmp <- data.frame(t(tmp), stringsAsFactors=FALSE)
  names(tmp) <- substring(names(tmp), 8, nchar(names(tmp)))
  names(tmp) <- gsub("\\.", "_", names(tmp))
}
