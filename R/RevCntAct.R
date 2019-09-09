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
#' growth_matrix_identity = RevCntAct(datall_revenue = total_revenue, type = c("Identity"), dates = dates)
#' }
#' @export

RevCntAct <- function(datall_revenue, type, dates){

  dat <- datall_revenue %>%
    ## change this filter to get result for different products
    filter(Product %in% type)%>%
    transmute(user_id=user_id,
              MRR= MRR,
              Effective_Start = as.Date(Effective_Start),
              Effective_End = as.Date(Effective_End))

  ## set up the matrix and fiil in values
  cnam <- c("New", "Retain", "Resurrected","Expansion", "Contraction", "Churn")
  grow_matrix <- matrix(0,nrow=length(dates), ncol= length(cnam)) %>%
    data.frame()
  names(grow_matrix) <- cnam

  for (i in 1:length(dates)){
    ####################################################################
    ############# This block of code is for the previous defition of MRR
    ############# based on the first day of each month
    # current month is from the 2nd day of last month to current day
    # int_start = dates[i] %m-% months(1) + days(1)
    # int_end = dates[i]
    # current = interval(int_start, int_end) # current month

    # previous month
    # int_start = dates[i] %m-% months(2) + days(1)
    # int_end = dates[i] %m-% months(1)
    # previous = interval(int_start, int_end)


    # all before until previous month
    # int_start = dates[i] %m-% months(12*100)
    # int_end = dates[i] %m-% months(2)
    # past = interval(int_start, int_end)
    ####################################################################

    int_start_current = floor_date(dates[i], unit = "month")
    int_end_current = dates[i]
    current = interval(int_start_current, int_end_current) # current month
    # calculate new revenue which is from new customer this month
    active_current <- dat %>%
      filter(Effective_Start <= dates[i]) %>%
      filter(is.na(Effective_End) | Effective_End > dates[i]) %>%
      group_by(user_id) %>%
      summarise(MRR = round(sum(MRR), 2)) %>%
      select(user_id, MRR_current = MRR)

    # previous month
    int_start_previous = floor_date(dates[i], unit = "month")  %m-% months(1)
    int_end_previous = ceiling_date(int_start_previous, unit = "month") - days(1)
    previous = interval(int_start_previous, int_end_previous)

    active_last_month <- dat %>%
      filter(Effective_Start <= int_end_previous) %>%
      filter(is.na(Effective_End) | Effective_End > int_end_previous) %>%
      group_by(user_id) %>%
      summarise(MRR = round(sum(MRR), 2)) %>%
      select(user_id, MRR_last_month = MRR)

    # all before until previous month
    int_start_past = dates[i] %m-% months(12*100)
    int_end_past = floor_date(dates[i], unit = "month")  %m-% months(1)
    int_end_past = int_end_past - days(1)
    past = interval(int_start_past, int_end_past)

    active_past <- dat %>%
      filter(Effective_Start <= int_end_past) %>%
      filter(is.na(Effective_End) | Effective_End > int_end_past) %>%
      group_by(user_id) %>%
      summarise(MRR = round(sum(MRR), 2)) %>%
      select(user_id, MRR_past = MRR)

    alltable <- merge(active_current, active_last_month, all = T) %>%
      merge(active_past, all=T)

    new <- alltable %>%
      # filter(is.na(MRR_past)&is.na(MRR_last_month)) %>%
      filter( (is.na(MRR_past) | MRR_past <= 0) &  (is.na(MRR_last_month) | MRR_last_month <= 0 ) ) %>%
      summarise(user_cnt = length(unique(user_id)))

    resurrected <- alltable %>%
      filter( (!is.na(MRR_current)) & MRR_current > 0  )%>%
      filter( is.na(MRR_last_month) | MRR_last_month <= 0) %>%
      filter( (!is.na(MRR_past)) & MRR_past > 0 ) %>%
      summarise(user_cnt = length(unique(user_id)))

    # an alternative way is to add up the smaller number from MRR_current and MRR_last_month
    retain1 <- alltable %>%
      filter( (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
      filter(MRR_current >= MRR_last_month) %>%
      summarise(user_cnt = length(unique(user_id)))

    retain2 <- alltable %>%
      filter( (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
      filter(MRR_current < MRR_last_month) %>%
      summarise(user_cnt = length(unique(user_id)))

    retain = retain1 + retain2

    expansion <- alltable %>%
      filter(   (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
      filter(MRR_current > MRR_last_month) %>%
      summarise(user_cnt = length(unique(user_id)))

    contraction <- alltable %>%
      filter( (!is.na(MRR_current)) &  MRR_current > 0  )%>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 ) %>%
      filter(MRR_current < MRR_last_month) %>%
      summarise(user_cnt = length(unique(user_id)))

    churn <- alltable %>%
      filter( is.na(MRR_current) | MRR_current <= 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 ) %>%
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
