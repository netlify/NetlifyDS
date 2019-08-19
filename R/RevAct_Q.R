#' Calculate revenue accounting matrix for each quarter
#' @description It calculates the accounting matrix given date, subscription data, and product type. It only returns one row for each quarter, i.e. it will sum up all the products.
#' @param dates  a date vector (have to be date type)
#' @param datall_revenue subscription data, need to have columns:  user_id, Product, Effective_Start, Effective_End, MRR
#' @param type product type
#' @return A revenue accounting matrix is returned for each date.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @export

RevAct_Q <- function(datall_revenue, type, dates){
  dat <- datall_revenue %>%
    # change this filter to get result for different products
    filter(Product %in% type)%>%
    transmute(user_id=user_id,
              MRR= MRR,
              Effective_Start = as.Date(Effective_Start),
              Effective_End = as.Date(Effective_End))

  cnam <- c("New", "Retain", "Resurrected","Expansion", "Contraction", "Churn")
  grow_matrix <- matrix(0,nrow=length(dates), ncol= length(cnam)) %>%
    data.frame()
  names(grow_matrix) <- cnam
  # i=10

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

    # current Q
    int_start = floor_date(dates[i], unit = "month") %m-% months(2)
    int_end = dates[i]
    current = interval(int_start, int_end) # current month

    # calculate new revenue which is from new customer this month
    active_current <- dat %>%
      filter(Effective_Start <= dates[i]) %>%
      filter(is.na(Effective_End) | Effective_End > dates[i]) %>%
      group_by(user_id) %>%
      summarise(MRR = round(sum(MRR), 2) ) %>%
      select(user_id, MRR_current = MRR)

    # previous Q
    int_end = floor_date(dates[i], unit = "month") %m-% months(2) - days(1)
    int_start = floor_date(int_end, unit = "month")  %m-% months(2)
    previous = interval(int_start, int_end)

    active_last_month <- dat %>%
      filter(Effective_Start <= int_end) %>%
      filter(is.na(Effective_End) | Effective_End > int_end) %>%
      group_by(user_id) %>%
      summarise(MRR = round(sum(MRR),2) ) %>%
      select(user_id, MRR_last_month = MRR)

    # all before until previous month
    int_end = dates[i]  %m-% months(6)
    int_start = dates[i]  %m-% months(1000)
    past = interval(int_start, int_end)

    active_past <- dat %>%
      filter(Effective_Start <= int_end) %>%
      filter(is.na(Effective_End) | Effective_End > int_end ) %>%
      group_by(user_id) %>%
      summarise(MRR = round(sum(MRR),2)) %>%
      select(user_id, MRR_past = MRR)

    alltable <- merge(active_current, active_last_month, all = T) %>%
      merge(active_past, all=T)

    new <- alltable %>%
      # filter(is.na(MRR_past)&is.na(MRR_last_month)) %>%
      filter( (is.na(MRR_past) | MRR_past <= 0) &  (is.na(MRR_last_month) | MRR_last_month <= 0 ) ) %>%
      summarise(MRR = sum(MRR_current, na.rm = T))

    resurrected <- alltable %>%
      # filter(!is.na(MRR_current))%>%
      # filter(is.na(MRR_last_month)) %>%
      # filter(!is.na(MRR_past)) %>%
      filter( (!is.na(MRR_current)) & MRR_current > 0  )%>%
      filter( is.na(MRR_last_month) | MRR_last_month <= 0) %>%
      filter( (!is.na(MRR_past)) & MRR_past > 0 ) %>%
      summarise(MRR = sum(MRR_current, na.rm = T))

    # an alternative way is to add up the smaller number from MRR_current and MRR_last_month
    retain1 <- alltable %>%
      # filter(!is.na(MRR_current)) %>%
      # filter(!is.na(MRR_last_month)) %>%
      filter( (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
      filter(MRR_current >= MRR_last_month) %>%
      summarise(MRR = sum(MRR_last_month, na.rm = T))

    retain2 <- alltable %>%
      # filter(!is.na(MRR_current)) %>%
      # filter(!is.na(MRR_last_month))%>%
      filter( (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
      filter(MRR_current < MRR_last_month) %>%
      summarise(MRR = sum(MRR_current, na.rm = T))

    retain = retain1 + retain2

    expansion <- alltable %>%
      # filter(!is.na(MRR_current)) %>%
      # filter(!is.na(MRR_last_month))%>%
      filter(   (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
      filter(MRR_current > MRR_last_month) %>%
      summarise(MRR = sum(MRR_current, na.rm = T) - sum(MRR_last_month, na.rm = T))

    contraction <- alltable %>%
      # filter(!is.na(MRR_current))%>%
      # filter(!is.na(MRR_last_month)) %>%
      filter( (!is.na(MRR_current)) &  MRR_current > 0  )%>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 ) %>%
      filter(MRR_current < MRR_last_month) %>%
      summarise(MRR = sum(MRR_last_month, na.rm = T) -sum(MRR_current, na.rm = T))

    churn <- alltable %>%
      # filter(is.na(MRR_current))%>%
      # filter(!is.na(MRR_last_month)) %>%
      filter( is.na(MRR_current) | MRR_current <= 0 ) %>%
      filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 ) %>%
      summarise(MRR = sum(MRR_last_month, na.rm = T))

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
