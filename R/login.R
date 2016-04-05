
#' Login to your Yahoo fantasy football home page
#'
#' Use your Yahoo credentials to login to your fantasy football home page of the form: http://football.fantasysports.yahoo.com/f1/some_number_here
#' @param league_id The ID that is at the end of your Yahoo fantasy football URL
#' @param username Your Yahoo account username or email address
#' @keywords login
#' @export
#' @return An rvest session
#' @examples
#' sess <- login(league_id=123456, username="my_yahoo_username", password="my_yahoo_password")
#' login
login <- function(league_id, username){
  if(nchar(league_id)!=6){stop("league_id must be six characters")}
  if(!is.character(username)){stop("username must be a character string")}
  
  uastring <- "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.87 Safari/537.36"
  url <- "http://football.fantasysports.yahoo.com/f1/"
  url <- paste0(url, league_id)
  
  s <- rvest::html_session(url, httr::user_agent(uastring))  
  myform <- rvest::html_form(s)[[1]]
  myform <- rvest::set_values(myform, username=username)
  s <- suppressWarnings(rvest::submit_form(s, myform, submit="signin"))
  s <- rvest::jump_to(s, s$response$url)
  myform <- rvest::html_form(s)[[1]]
  if("code" %in% names(myform$fields)){
    code <- readline(prompt="In your Yahoo app, find and click on the Account Key icon.\nGet the 8 character code and\nenter it here: ")
  }else{
    print("Unable to login")
    return(NULL)
  }
  myform <- rvest::set_values(myform, code=code)  
  s <- suppressWarnings(rvest::submit_form(s, myform, submit="verify"))
  if(grepl("authorize\\/verify", s$url)){
    print("Wrong code entered, unable to login")
    return(NULL)
  }else{
    print("Login successful")
  }
  s <- rvest::jump_to(s, s$response$url)
  return(s)
}
    
