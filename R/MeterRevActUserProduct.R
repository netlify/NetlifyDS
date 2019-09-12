#' Calculate metered revenue accounting matrix per day, user and product
#' @description It calculates the accounting matrix for each user and product given date, subscription data and product category.
#' @param dates  a date vector (have to be date type)
#' @param datall_revenue subscription data, need to have columns:  user_id, Product, Effective_Start, one_time_charge
#' @param type a vector of product names
#' @return A metered revenue accounting matrix is returned for each day, user and product
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @export

MeterRevActUserProduct <- function(datall_revenue, dates, type){

  dat <- datall_revenue %>%
    ## change this filter to get result for different products
    filter(Product %in% type)%>%
    transmute(user_id=user_id,
              one_time_charge = one_time_charge,
              Effective_Start = as.Date(Effective_Start))

  # function to impute 0 to missing value
  impute0 = function(dat) {
    for (i in 2:ncol(dat)){
      idx = which(is.na(dat[,i]))

      if(length(idx)>0){
        dat[idx,i] = 0
      }
    }
    return(dat)
  }

  res = NULL
  for (i in 1:length(dates)){
    # current month is from the 1nd day to the last day each month
    int_start = floor_date(dates[i], unit = "month")
    int_end = dates[i]
    current = interval(int_start, int_end) # current month

    # calculate new revenue which is from new customer this month
    active_current <- dat %>%
      filter(Effective_Start %within% current) %>%
      group_by(user_id) %>%
      summarise(one_time_charge = round(sum(one_time_charge), 2)) %>%
      select(user_id, one_time_charge_current = one_time_charge)

    # previous month
    int_start = floor_date(dates[i], unit = "month")  %m-% months(1)
    int_end = ceiling_date(int_start, unit = "month") - days(1)
    previous = interval(int_start, int_end)

    active_last_month <- dat %>%
      filter(Effective_Start %within% previous) %>%
      group_by(user_id) %>%
      summarise(one_time_charge = round(sum(one_time_charge), 2)) %>%
      select(user_id, one_time_charge_last_month = one_time_charge)

    # all before until previous month
    int_start = dates[i] %m-% months(12*100)
    int_end = floor_date(dates[i], unit = "month")  %m-% months(1)
    int_end = int_end - days(1)
    past = interval(int_start, int_end)

    active_past <- dat %>%
      filter(Effective_Start %within% past) %>%
      group_by(user_id) %>%
      summarise(one_time_charge = round(sum(one_time_charge), 2)) %>%
      select(user_id, one_time_charge_past = one_time_charge)

    alltable <- merge(active_current, active_last_month, all = T) %>%
      merge(active_past, all=T)

    ########################## Break out the revenue ###########################

    # current month is from the 1nd day to the last day each month

    new <- alltable %>%
      filter( (is.na(one_time_charge_past) | one_time_charge_past <= 0) &  (is.na(one_time_charge_last_month) | one_time_charge_last_month <= 0 ) ) %>%
      select(user_id, new = one_time_charge_current )

    resurrected <- alltable %>%
      filter( (!is.na(one_time_charge_current)) & one_time_charge_current > 0  )%>%
      filter( is.na(one_time_charge_last_month) | one_time_charge_last_month <= 0) %>%
      filter( (!is.na(one_time_charge_past)) & one_time_charge_past > 0 ) %>%
      select(user_id,resurrected = one_time_charge_current)

    # an alternative way is to add up the smaller number from MRR_current and MRR_last_month
    retain1 <- alltable %>%
      filter( (!is.na(one_time_charge_current)) &  one_time_charge_current > 0 ) %>%
      filter( (!is.na(one_time_charge_last_month)) & one_time_charge_last_month > 0 )%>%
      filter(one_time_charge_current >= one_time_charge_last_month) %>%
      select(user_id,retain1 = one_time_charge_last_month)

    retain2 <- alltable %>%
      filter( (!is.na(one_time_charge_current)) &  one_time_charge_current > 0 ) %>%
      filter( (!is.na(one_time_charge_last_month)) & one_time_charge_last_month > 0 )%>%
      filter(one_time_charge_current < one_time_charge_last_month) %>%
      select(user_id,retain2 = one_time_charge_current)

    retain = merge(retain1, retain2, all=T) %>%
      impute0()%>%
      transmute(user_id = user_id, retain = retain1 + retain2)

    expansion <- alltable %>%
      filter(   (!is.na(one_time_charge_current)) &  one_time_charge_current > 0 ) %>%
      filter( (!is.na(one_time_charge_last_month)) & one_time_charge_last_month > 0 )%>%
      filter(one_time_charge_current > one_time_charge_last_month) %>%
      impute0()%>%
      transmute(user_id = user_id, expansion = one_time_charge_current-one_time_charge_last_month)

    contraction <- alltable %>%
      filter( (!is.na(one_time_charge_current)) &  one_time_charge_current > 0  )%>%
      filter( (!is.na(one_time_charge_last_month)) & one_time_charge_last_month > 0 ) %>%
      filter(one_time_charge_current < one_time_charge_last_month) %>%
      impute0()%>%
      transmute(user_id = user_id, contraction =one_time_charge_last_month - one_time_charge_current)

    churn <- alltable %>%
      filter( is.na(one_time_charge_current) | one_time_charge_current <= 0 ) %>%
      filter( (!is.na(one_time_charge_last_month)) & one_time_charge_last_month > 0 ) %>%
      transmute(user_id = user_id, churn =one_time_charge_last_month)

    res0 = merge(new, resurrected, all = T)
    res0 = merge(res0, retain, all = T)
    res0 = merge(res0, expansion, all = T)
    res0 = merge(res0, contraction, all = T)
    res0 = merge(res0, churn, all = T)
    res0 = impute0(res0)

    res0$date = dates[i]

    res = rbind(res, res0)
    res = res %>% filter( !(new == 0 &  resurrected == 0 & retain == 0 &  expansion == 0 & contraction == 0 & churn == 0) )
  }
  return(res)
}
