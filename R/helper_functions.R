create_prize_df <- function(prizes_df) {
  
  if(sum(names(prizes_df) %in% c("minPosition","maxPosition","tierPayoutDescriptions")) != 3)
    stop("prizes_df must contain the columns: minPosition, maxPosition, tierPayoutDescriptions")
  
  df <- prizes_df
  df$winnings <- readr::parse_number(prizes_df$tierPayoutDescriptions$Cash)
  
  df <- df %>% dplyr::select(-tierPayoutDescriptions)
  
  create_running_payout <- function(df) {
    
    rep(df$winnings, length(df$minPosition:df$maxPosition))
    
  }
  
  all_payouts <- 
    df %>% 
    purrr::transpose() %>% 
    purrr::map(create_running_payout) %>%
    unlist()
  
  prizes <- data.frame(rank = 1:length(all_payouts),
                       prize = all_payouts)
  
  return(prizes)
  
}
