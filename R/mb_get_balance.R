mb_get_balance <- function(session_data)
{
  content            <- list(status_code=0)
  valid_states       <- c("active","pending")
  if(is.null(session_data)|!is.list(session_data)){
    print(paste("You have not provided valid data about your session in the session_data parameter. Please execute mb_login('my_user_name','verysafepassword') and save the resulting object in a variable e.g. my_session <- mb_login(username,pwd); and pass session_data=my_session as a parameter in this function."));return(content)
  }  
  get_balance_resp    <- httr::GET("https://api.matchbook.com/edge/rest/account/balance",httr::set_cookies('session-token'=session_data$session_token),httr::add_headers('User-Agent'='rlibnf'))
  status_code        <- get_balance_resp$status_code
  if(status_code==200)
  {
    content <- jsonlite::fromJSON(content(get_balance_resp, "text", "application/json"))
  } else
  {
    print(paste("Warning/Error in communicating with https://www.matchbook.com/bpapi/rest/lookups/sports",sep=""))
    content$status_code <- status_code
  }
  return(content)
}
