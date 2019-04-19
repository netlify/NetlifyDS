#' Calculate MRR for a specific date and product type
#' @description It calculates the MRR given date, subscription date, and product type
#' @param time_point  a date value (have to be date type)
#' @param subs subscription data
#' @param type product type
#' @return
#' a number of MRR is returned
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' dates <- seq(as.Date("2018-02-01"), lubridate::ceiling_date(Sys.Date(), unit = "month"), "months") - days(1)
#' dates
#' add_ons_mrr <- sapply(dates, function(time_point) {mrrAtDateType(time_point, datall_revenue, add_ons)})
#' @export
#'

mrrAtDateType <- function(time_point, subs, type) {
  # time_point: time of mrr
  # subs: subscription data
  time_point <- as.Date(time_point)
  actives <- subs  %>%
    dplyr::filter(Product %in% type) %>%
    dplyr::filter(Effective_Start <= time_point) %>%
    dplyr::filter(is.na(Effective_End) | Effective_End >= time_point)
  return(sum(actives$MRR , na.rm=T))
}
