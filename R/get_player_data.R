#' Get a list of players in your league
#'
#' This does not use the API, it just reads the webpage with the player data on it and formats a dataframe with the results
#' @param n The number of players to request in multiples of 25
#' @param by_position TRUE/FALSE indicating whether to download the data by position. A value of TRUE would download n players for each position. A value of FALSE would download n total players.
#' @keywords players
#' @export
#' @return A dataframe containing the player data
#' @examples
#' player_df <- get_player_data(n=25)
#' player_df <- get_player_data(n=25, by_position=TRUE)
#' get_player_data
get_player_data <- function(n=250, by_position=FALSE){
  if(!is.numeric(n)){stop("n must be numeric")}
  if(!is.logical(by_position)){stop("by_position must be logical")}
  
  positions <- c("QB", "WR", "TE", "RB") #, "DEF", "K")
  
  format_df <- function(df){
    df <- df[, -c(1,3,4,ncol(df))]
    names(df) <- c("player_info", "games_played", "fan_pts", 
                   "perc_owned", "rank_proj", "rank_act", "pass_yds", 
                   "pass_td", "pass_int", "pass_picksix", "pass_40ydtd", 
                   "rush_att", "rush_yds", "rush_td", "rush_40ydatt", 
                   "rush_40tdtd", "rec_tgt", "rec_rec", "rec_yds", 
                   "rec_td", "rec_40tdrec", "rec_40ydtd","return_yds", 
                   "return_td", "twopts", "fum_lost")
    df <- df[-1, ]
    df$player_name <- stringr::str_extract(df$player_info, 
      "[A-Z]{1}\\.\\s[A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)")
    df$team_abb <- stringr::str_extract(df$player_info, 
      "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)")
    df$position <- stringr::str_extract(df$player_info, 
      paste0("(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"))
    return(df)
  }
  
  
  df_all <- NULL
  n <- 25*(n%/%25 + as.logical(n%%25)) # round up to nearest 25
  url <- "http://football.fantasysports.yahoo.com/f1/50878/players"
  for(position in (if(by_position)positions else "O")){
    for(i in 1:(n/25)){
      params <- list("status"="A",
                     "pos"=position,
                     "cut_type"="9",
                     "stat1"="S_S_2015",
                     "myteam"="0",
                     "sort"="PR",
                     "sdir"="1",
                     "count"=((i * 25) - 25)
      )
      a <- httr::GET(url, query=params)
      a_html <- httr::content(a, "text")
      tmp <- xml2::read_html(a_html)
      df <- rvest::html_table(tmp)[[2]]
      df <- format_df(df)
      df_all <- rbind(df_all, df)
    }
  }
  return(df_all)
}
