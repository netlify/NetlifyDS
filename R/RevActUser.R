## this is the function to get revenue accounting matrix
## user level
## column required: user_id, MRR, Effective_Start,  Effective_End

RevActUser <- function(datall_revenue, type, dates){
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

    # current month is from the 1nd day to the last day each month
    int_start = floor_date(dates[i], unit = "month")
    int_end = dates[i]
    current = interval(int_start, int_end) # current month

    # calculate new revenue which is from new customer this month
    active_current <- dat %>%
      filter(Effective_Start <= dates[i]) %>%
      filter(is.na(Effective_End) | Effective_End >= dates[i]) %>%
      group_by(user_id) %>%
      summarise(MRR = sum(MRR)) %>%
      select(user_id, MRR_current = MRR)

    # previous month
    int_start = floor_date(dates[i], unit = "month")  %m-% months(1)
    int_end = ceiling_date(int_start, unit = "month") - days(1)
    previous = interval(int_start, int_end)

    active_last_month <- dat %>%
      filter(Effective_Start <= int_end) %>%
      filter(is.na(Effective_End) | Effective_End >= int_end) %>%
      group_by(user_id) %>%
      summarise(MRR = sum(MRR)) %>%
      select(user_id, MRR_last_month = MRR)

    # all before until previous month
    int_start = dates[i] %m-% months(12*100)
    int_end = floor_date(dates[i], unit = "month")  %m-% months(1)
    int_end = int_end - days(1)
    past = interval(int_start, int_end)

    active_past <- dat %>%
      filter(Effective_Start <= int_end) %>%
      filter(is.na(Effective_End) | Effective_End >= int_end ) %>%
      group_by(user_id) %>%
      summarise(MRR = sum(MRR)) %>%
      select(user_id, MRR_past = MRR)

    alltable <- merge(active_current, active_last_month, all = T) %>%
      merge(active_past, all=T)

    new <- alltable %>%
      filter(is.na(MRR_past)&is.na(MRR_last_month)) %>%
      summarise(MRR = sum(MRR_current, na.rm = T))

    resurrected <- alltable %>%
      filter(!is.na(MRR_current))%>%
      filter(is.na(MRR_last_month)) %>%
      filter(!is.na(MRR_past)) %>%
      summarise(MRR = sum(MRR_current, na.rm = T))

    # an alternative way is to add up the smaller number from MRR_current and MRR_last_month
    retain1 <- alltable %>%
      filter(!is.na(MRR_current)) %>%
      filter(!is.na(MRR_last_month))%>%
      filter(MRR_current >= MRR_last_month) %>%
      summarise(MRR = sum(MRR_last_month, na.rm = T))

    retain2 <- alltable %>%
      filter(!is.na(MRR_current)) %>%
      filter(!is.na(MRR_last_month))%>%
      filter(MRR_current < MRR_last_month) %>%
      summarise(MRR = sum(MRR_current, na.rm = T))

    retain = retain1 + retain2

    expansion <- alltable %>%
      filter(!is.na(MRR_current)) %>%
      filter(!is.na(MRR_last_month))%>%
      filter(MRR_current > MRR_last_month) %>%
      summarise(MRR = sum(MRR_current-MRR_last_month, na.rm = T))

    contraction <- alltable %>%
      filter(!is.na(MRR_current))%>%
      filter(!is.na(MRR_last_month)) %>%
      filter(MRR_current < MRR_last_month) %>%
      summarise(MRR = sum(MRR_last_month-MRR_current, na.rm = T))

    churn <- alltable %>%
      filter(is.na(MRR_current))%>%
      filter(!is.na(MRR_last_month)) %>%
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
