#' Get a list of players in the NFL
#'
#' This does not use the API, it just reads the webpage with the player data on it and formats a dataframe with the results
#' @param session A session returned from the login function
#' @param n The number of players to request in multiples of 25
#' @param by_position TRUE/FALSE indicating whether to download the data by position. A value of TRUE would download n players for each position and return a list of dataframes. A value of FALSE would download n total offensive players and return a single dataframe.
#' @param year The year you want data for. 2013, 2014, 2015 available
#' @keywords players
#' @export
#' @return A dataframe or a list of dataframes containing the player data
#' @examples
#' player_df <- get_player_data(session=session, n=25, year="2015")
#' player_df_list <- get_player_data(session=session, n=25, by_position=TRUE, year="2015")
#' get_player_data
get_player_data <- function(session, n=250, by_position=FALSE, year){
  if(!"session" %in% class(session)){stop("session must be of class session")}
  if(!is.numeric(n)){stop("n must be numeric")}
  if(!is.logical(by_position)){stop("by_position must be logical")}
  if(!as.character(year) %in% c("2013", "2014", "2015")){
    stop("year must be one of 2013, 2014, or 2015")
  }
  
  positions <- c("QB", "WR", "TE", "RB", "DEF", "K")
  
  format_df_off <- function(df){
    df <- df[, -c(1,3,ncol(df))]
    names(df) <- c("player_info", "owner", "games_played", "fan_pts", 
                   "perc_owned", "rank_proj", "rank_act", "pass_yds", 
                   "pass_td", "pass_int", 
                   "rush_att", "rush_yds", "rush_td", 
                   "rec_tgt", "rec_rec", "rec_yds", 
                   "rec_td", 
                   "return_td", "twopts", "fum_lost")
    df <- df[-1, ]
    df$player_name <- stringr::str_extract(df$player_info, 
                                           "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)")
    df$team_abb <- stringr::str_extract(df$player_info, 
                                        "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)")
    df$position <- stringr::str_extract(df$player_info, 
                                        "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+")
    return(df)
  }
  
  format_df_def <- function(df){
    df <- df[, -c(1,3,ncol(df))]
    names(df) <- c("player_info", "owner", "games_played", "fan_pts", 
                   "perc_owned", "rank_proj", "rank_act", "pts_vs", 
                   "sack", "safe", "int", "fum_rec", 
                   "td", "blk_kick", "ret_td")
    df <- df[-1, ]
    df$player_name <- stringr::str_extract(df$player_info,
                                           "(?<=\\s)[A-Z]{1}[a-z]{1,}[A-Za-z ]*(?=\\s[A-Z]{1}[a-zA-Z]{1,2})")
    df$team_abb <- stringr::str_extract(df$player_info, 
                                        "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)")
    df$position <- stringr::str_extract(df$player_info, 
                                        "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+")
    return(df)
  }
  
  format_df_k <- function(df){
    df <- df[, -c(1,3,ncol(df))]
    names(df) <- c("player_info", "owner", "games_played", "fan_pts", 
                   "perc_owned", "rank_proj", "rank_act", "fg_made_0_19", 
                   "fg_made_20_29", "fg_made_30_39", "fg_made_40_49", 
                   "fg_made_50plus", "pat_made")
    df <- df[-1, ]
    df$player_name <- stringr::str_extract(df$player_info, 
                                           "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)")
    df$team_abb <- stringr::str_extract(df$player_info, 
                                        "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)")
    df$position <- stringr::str_extract(df$player_info, 
                                        "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+")
    return(df)
  }
  
  df_all <- list()
  n <- 25*(n%/%25 + as.logical(n%%25)) # round up to nearest 25
  url <- paste0(session$handle$url, "/players")
  for(position in (if(by_position)positions else "O")){
    for(i in 1:(n/25)){
      params <- list("status"="ALL",
                     "pos"=position,
                     "stat1"=paste0("S_S_", year),
                     "sort"="PR",
                     "sdir"="1",
                     "count"=((i * 25) - 25)
      )
      a <- rvest::jump_to(session, url=httr::modify_url(url, query=params))
      tmp <- xml2::read_html(a)
      df <- rvest::html_table(a)[[2]]
      if(position %in% c("QB", "WR", "RB", "TE", "O")) df <- format_df_off(df)
      if(position == "DEF") df <- format_df_def(df)
      if(position == "K") df <- format_df_k(df)
      df_all[[position]] <- df
    }
  }
  if(length(df_all) == 1){
    return(df_all[[1]])
  }else{
    return(df_all)
  }
}
