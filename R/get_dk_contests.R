get_dk_contests <- function(sport) {
  
  # sports: GOLF,
  
  response <- 
    httr::GET(
      url = "https://www.draftkings.com/lobby/getcontests?",
      query = list(
        sport = sport),
      httr::user_agent("dkUser")
    )
  
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

