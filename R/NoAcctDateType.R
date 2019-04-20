## function to calculate numbuer of accounts
NoAcctDateType <- function(time_point, subs, type) {
  time_point <- as.Date(time_point)
  # time_point: time of mrr
  # subs: subscription data
  actives <- subs  %>%
    filter(Product %in% type) %>%
    filter(Effective_Start <= time_point) %>%
    filter( is.na(Effective_End) | Effective_End >= time_point) %>%
    # only keep those account with positive mrr
    filter(MRR > 0)
  # return the number of accounts
  return(length(unique(actives$account_id)))
}
