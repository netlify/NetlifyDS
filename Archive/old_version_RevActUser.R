## this is the old version of RevActUser
## this is the function to get revenue accounting matrix
## user level
## column required: user_id, MRR, Effective_Start,  Effective_End

RevActUser <- function(datall_revenue, dates){

  dat <- total_revenue %>%
    # change this filter to get result for different products
    # filter(Product %in% type)%>%
    transmute(user_id=user_id,
              MRR= MRR,
              Effective_Start = as.Date(Effective_Start),
              Effective_End = as.Date(Effective_End))

  res = NULL
  for (i in 1:length(dates)){
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
      select(user_id,new = MRR_current)

    resurrected <- alltable %>%
      filter(!is.na(MRR_current))%>%
      filter(is.na(MRR_last_month)) %>%
      filter(!is.na(MRR_past)) %>%
      select(user_id,resurrected = MRR_current)

    # an alternative way is to add up the smaller number from MRR_current and MRR_last_month
    retain1 <- alltable %>%
      filter(!is.na(MRR_current)) %>%
      filter(!is.na(MRR_last_month))%>%
      filter(MRR_current >= MRR_last_month) %>%
      select(user_id,retain1 = MRR_last_month)

    retain2 <- alltable %>%
      filter(!is.na(MRR_current)) %>%
      filter(!is.na(MRR_last_month))%>%
      filter(MRR_current < MRR_last_month) %>%
      select(user_id,retain2 = MRR_current)

    retain = merge(retain1, retain2, all=T) %>%
      impute0()%>%
      transmute(user_id = user_id, retain = retain1 + retain2)

    expansion <- alltable %>%
      filter(!is.na(MRR_current)) %>%
      filter(!is.na(MRR_last_month))%>%
      filter(MRR_current > MRR_last_month) %>%
      impute0()%>%
      transmute(user_id = user_id, expansion = MRR_current-MRR_last_month)

    contraction <- alltable %>%
      filter(!is.na(MRR_current))%>%
      filter(!is.na(MRR_last_month)) %>%
      filter(MRR_current < MRR_last_month) %>%
      impute0()%>%
      transmute(user_id = user_id, contraction =MRR_last_month-MRR_current)

    churn <- alltable %>%
      filter(is.na(MRR_current))%>%
      filter(!is.na(MRR_last_month)) %>%
      transmute(user_id = user_id, churn =MRR_last_month)

    res0 = merge(new, resurrected, all = T)
    res0 = merge(res0, retain, all = T)
    res0 = merge(res0, expansion, all = T)
    res0 = merge(res0, contraction, all = T)
    res0 = merge(res0, churn, all = T)
    res0 = impute0(res0)

    res0$date = dates[i]

    res = rbind(res, res0)
  }

  return(res)
}
