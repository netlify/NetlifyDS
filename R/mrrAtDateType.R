#' Calculate MRR for a specific date and product type
#' @description It calculates the MRR given date, subscription data, and product type
#' @param time_point  a date value (have to be date type)
#' @param subs subscription data, need to have columns: account_id, user_id, Product, Effective_Start, Effective_End, MRR
#' @param type product type, if "NULL", it will return MRR for each product
#' @return
#' a vector of MRR is returned
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' add_ons <- c("Forms", "Functions", "Identity", "Domains", "Large Media")
#' dates <- seq(as.Date("2018-02-01"), lubridate::ceiling_date(Sys.Date(), unit = "month"), "months") - days(1)
#' dates
#' add_ons_mrr <- sapply(dates, function(time_point) {mrrAtDateType(time_point, datall_revenue, add_ons)})
#' }
#' @export
#'

mrrAtDateType <- function(time_point, subs, type = NULL) {
  # time_point: time of mrr
  # subs: subscription data
  time_point <- as.Date(time_point)

  if (is.null(type)){
    actives <- subs  %>%
      dplyr::filter(Effective_Start <= time_point) %>%
      dplyr::filter( is.na(Effective_End) | Effective_End >= time_point)

    res <- actives %>%
      dplyr::group_by(account_id,user_id,Product) %>%
      dplyr::summarise(MRR = sum(MRR , na.rm=T))

    res$Date = time_point
    return(res)
  }
  else{
    actives <- subs  %>%
      dplyr::filter(Product %in% type) %>%
      dplyr::filter(Effective_Start <= time_point) %>%
      dplyr::filter(is.na(Effective_End) | Effective_End >= time_point)
    return(sum(actives$MRR , na.rm=T))
  }

}
