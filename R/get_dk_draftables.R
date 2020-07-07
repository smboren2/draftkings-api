get_dk_draftables <- function(DraftGroupId) {
  
  base <- "https://api.draftkings.com/draftgroups/v1/draftgroups/"
  draftgroup <- DraftGroupId
  suffix <- "/draftables?format=json"
  
  api_url <- paste0(base, draftgroup, suffix)
  
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
