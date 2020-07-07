get_dk_contest_detail <- function(contestid) {
  
  # https://api.draftkings.com/contests/v1/contests/88049070?format=json
  
  base <- "https://api.draftkings.com/contests/v1/contests/"
  id <- contestid
  suffix <- "?format=json"
  
  api_url <- paste0(base, id, suffix)
  
  response <- httr::GET(url = api_url, httr::user_agent("dkUser"))
  
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  if (httr::http_error(response)) {
    message(
      sprintf("DraftKings API request failed [%s Response], no data returned",
              httr::status_code(response))
    )
  }
  
  parsed <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  return(parsed)
  
}
