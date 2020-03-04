#' Calculate revenue accounting matrix
#' @description It calculates the accounting matrix given date and daily revenue data. It only returns one row for each date, i.e. it will sum up all the products.
#' @param dates  a date vector (have to be date type)
#' @param datall_revenue daily revenue data, need to have columns:  user_id, Product, active_date, MRR
#' @param type product type
#' @return A revenue accounting matrix is returned for each date.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' dates <- seq(as.Date("2018-02-01"), lubridate::ceiling_date(Sys.Date(), unit = "month"), "months") - days(1)
#' dates
#' # need to get daily revenue from subscription data if you don't have that already
#' base_monthly = do.call("rbind", lapply(dates, function(time_point)
#' growth_matrix_identity = RevAct(datall_revenue = base_monthly, type = c("Identity"), dates = dates)
#' }
#' @export


RevAct <- function(datall_revenue, type, dates){


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
      summarise(MRR = sum(cmrr, na.rm = T))

    # Get reesurrected
    resurrected <- alltable %>%
      filter(cmrr > 0 & pmrr == 0 & lmrr > 0) %>%
      summarise(MRR = sum(cmrr, na.rm = T))

    # Get Retain
    retain1 <- alltable %>%
      filter(cmrr > 0 & pmrr > 0) %>%
      filter(cmrr >= pmrr) %>%
      summarise(MRR = sum(pmrr, na.rm = T))

    retain2 <- alltable %>%
      filter(cmrr > 0 & pmrr > 0) %>%
      filter(cmrr < pmrr) %>%
      summarise(MRR = sum(cmrr, na.rm = T))

    retain = retain1 + retain2

    # Get expansion
    expansion <- alltable %>%
      filter(cmrr > 0 & pmrr >0) %>%
      filter(cmrr > pmrr) %>%
      summarise(MRR = sum(cmrr, na.rm = T) - sum(pmrr, na.rm = T) )

    # Get contraction
    contraction <- alltable %>%
      filter(cmrr > 0 & pmrr >0) %>%
      filter(cmrr < pmrr) %>%
      summarise(MRR = sum(pmrr, na.rm = T)-sum(cmrr, na.rm = T))

    # Get churn
    churn <- alltable %>%
      filter(cmrr == 0 & pmrr > 0) %>%
      summarise(MRR = sum(pmrr, na.rm = T))

    grow_matrix[i, "New"] <- new
    grow_matrix[i, "Retain"] <- retain
    grow_matrix[i, "Resurrected"] <- resurrected
    grow_matrix[i, "Expansion"] <- expansion
    grow_matrix[i, "Contraction"] <- contraction
    grow_matrix[i, "Churn"] <- churn
  }
  grow_matrix$dates <- dates

  grow_matrix <- grow_matrix %>%
    mutate(MRR = New + Retain + Resurrected + Expansion,
           ARR = MRR*12) %>%
    mutate(QuickRatio = round((New + Resurrected+ Expansion)/(Churn+Contraction),2))

  return(grow_matrix)
}
