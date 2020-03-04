#' Calculate revenue accounting matrix by counts
#' @description It calculates the accounting matrix by counts given date and subscription data. It only returns one row for each date, i.e. it will sum up all the products.
#' @param dates  a date vector (have to be date type)
#' @param datall_revenue subscription data, need to have columns:  user_id, Product, Effective_Start, Effective_End, MRR
#' @param type product type
#' @return A revenue accounting matrix by count is returned for each date.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' dates <- seq(as.Date("2018-02-01"), lubridate::ceiling_date(Sys.Date(), unit = "month"), "months") - days(1)
#' dates
#' # need to get daily revenue from subscription data if you don't have that already
#' base_monthly = do.call("rbind", lapply(dates, function(time_point)
#' {NetlifyDS::mrrAtDateType(time_point, subs = base_monthly, type = NULL)})
#' growth_matrix_identity = RevCntAct(datall_revenue = total_revenue, type = c("Identity"), dates = dates)
#' }
#' @export

RevCntAct <- function(datall_revenue, type, dates){

  dat <- datall_revenue %>%
    ungroup()%>%
    ## change this filter to get result for different products
    filter(Product %in% type) %>%
    transmute(user_id = user_id,
              MRR= MRR,
              active_date = as.Date(active_date)) %>%
    group_by(user_id, active_date) %>%
    summarise(MRR = round(sum(MRR), 2)) %>%
    ungroup()

  cnam <- c("New", "Retain", "Resurrected","Expansion", "Contraction", "Churn")
  grow_matrix <- matrix(0,nrow=length(dates), ncol= length(cnam)) %>%
    data.frame()
  names(grow_matrix) <- cnam

  for (i in 1:length(dates)){
    # the current month
    cmon = dates[i]

    # the previous month
    pmon = floor_date(dates[i], unit = "month")- days(1)

    # the legacy month
    lmon = dates[dates < pmon]

    # current mrr
    cmrr = dat %>%
      filter(active_date == cmon) %>%
      select(user_id, cmrr = MRR) %>%
      filter(cmrr > 0)

    # previous mrr
    pmrr = dat %>%
      filter(active_date == pmon) %>%
      select(user_id, pmrr = MRR) %>%
      filter(pmrr > 0)

    # legacy mrr, get the maximum number
    lmrr = dat %>%
      filter(active_date %in% lmon) %>%
      group_by(user_id) %>%
      summarise(lmrr = max(MRR)) %>%
      filter(lmrr > 0)


    # join all together
    alltable <- merge(cmrr, pmrr, all = T) %>%
      merge(lmrr, all=T) %>%
      NetlifyDS::impute_dat(method = "zero")

    # Get new
    new = alltable %>%
      filter( cmrr > 0 & pmrr == 0 & lmrr == 0) %>%
      summarise(user_cnt = length(unique(user_id)))

    # Get reesurrected
    resurrected <- alltable %>%
      filter(cmrr > 0 & pmrr == 0 & lmrr > 0) %>%
      summarise(user_cnt = length(unique(user_id)))

    # Get Retain
    retain1 <- alltable %>%
      filter(cmrr > 0 & pmrr > 0) %>%
      filter(cmrr >= pmrr) %>%
      summarise(user_cnt = length(unique(user_id)))

    retain2 <- alltable %>%
      filter(cmrr > 0 & pmrr > 0) %>%
      filter(cmrr < pmrr) %>%
      summarise(user_cnt = length(unique(user_id)))

    retain = retain1 + retain2

    # Get expansion
    expansion <- alltable %>%
      filter(cmrr > 0 & pmrr >0) %>%
      filter(cmrr > pmrr) %>%
      summarise(user_cnt = length(unique(user_id)) )

    # Get contraction
    contraction <- alltable %>%
      filter(cmrr > 0 & pmrr >0) %>%
      filter(cmrr < pmrr) %>%
      summarise(user_cnt = length(unique(user_id)))

    # Get churn
    churn <- alltable %>%
      filter(cmrr == 0 & pmrr > 0) %>%
      summarise(user_cnt = length(unique(user_id)))

    grow_matrix[i, "New"] <- new
    grow_matrix[i, "Retain"] <- retain
    grow_matrix[i, "Resurrected"] <- resurrected
    grow_matrix[i, "Expansion"] <- expansion
    grow_matrix[i, "Contraction"] <- contraction
    grow_matrix[i, "Churn"] <- churn
  }
  grow_matrix$dates <- dates

  return(grow_matrix)
}

