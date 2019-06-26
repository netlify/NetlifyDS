#' Calculate revenue accounting matrix for each day and user
#' @description It calculates the accounting matrix given date, subscription data.
#' @param dates  a date vector (have to be date type)
#' @param datall_revenue subscription data, need to have columns:  user_id, Effective_Start, Effective_End, MRR
#' @return A revenue accounting matrix is returned for each day and user
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' res = RevActUser(total_revenue, dates)
#' }
#' @export


RevActUser <- function(datall_revenue, dates){

  dat <- total_revenue %>%
    # change this filter to get result for different products
    # filter(Product %in% type)%>%
    transmute(user_id=user_id,
              MRR= MRR,
              Effective_Start = as.Date(Effective_Start),
              Effective_End = as.Date(Effective_End))

  res_test = do.call("rbind",
                     lapply(dates, function(time_point){

                       # current month is from the 1nd day to the last day each month
                       int_start = floor_date(time_point, unit = "month")
                       int_end = time_point
                       current = interval(int_start, int_end) # current month

                       # calculate new revenue which is from new customer this month
                       active_current <- dat %>%
                         filter(Effective_Start <= time_point) %>%
                         filter(is.na(Effective_End) | Effective_End >= time_point) %>%
                         group_by(user_id) %>%
                         summarise(MRR = round(sum(MRR), 10)) %>%
                         select(user_id, MRR_current = MRR)

                       # previous month
                       int_start = floor_date(time_point, unit = "month")  %m-% months(1)
                       int_end = ceiling_date(int_start, unit = "month") - days(1)
                       previous = interval(int_start, int_end)

                       active_last_month <- dat %>%
                         filter(Effective_Start <= int_end) %>%
                         filter(is.na(Effective_End) | Effective_End >= int_end) %>%
                         group_by(user_id) %>%
                         summarise(MRR = round(sum(MRR), 10)) %>%
                         select(user_id, MRR_last_month = MRR)

                       # all before until previous month
                       int_start = time_point %m-% months(12*100)
                       int_end = floor_date(time_point, unit = "month")  %m-% months(1)
                       int_end = int_end - days(1)
                       past = interval(int_start, int_end)

                       active_past <- dat %>%
                         filter(Effective_Start <= int_end) %>%
                         filter(is.na(Effective_End) | Effective_End >= int_end ) %>%
                         group_by(user_id) %>%
                         summarise(MRR = round(sum(MRR), 10)) %>%
                         select(user_id, MRR_past = MRR)

                       alltable <- merge(active_current, active_last_month, all = T) %>%
                         merge(active_past, all=T)

                       ########################## Break out the revenue ###########################

                       # current month is from the 1nd day to the last day each month

                       new <- alltable %>%
                         filter( (is.na(MRR_past) | MRR_past <= 0) &  (is.na(MRR_last_month) | MRR_last_month <= 0 ) ) %>%
                         select(user_id, new = MRR_current )

                       resurrected <- alltable %>%
                         filter( (!is.na(MRR_current)) & MRR_current > 0  )%>%
                         filter( is.na(MRR_last_month) | MRR_last_month <= 0) %>%
                         filter( (!is.na(MRR_past)) & MRR_past > 0 ) %>%
                         select(user_id,resurrected = MRR_current)

                       # an alternative way is to add up the smaller number from MRR_current and MRR_last_month

                       retain1 <- alltable %>%
                         filter( (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
                         filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
                         filter(MRR_current >= MRR_last_month) %>%
                         select(user_id,retain1 = MRR_last_month)

                       retain2 <- alltable %>%
                         filter( (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
                         filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
                         filter(MRR_current < MRR_last_month) %>%
                         select(user_id,retain2 = MRR_current)

                       retain = merge(retain1, retain2, all=T) %>%
                         impute0()%>%
                         transmute(user_id = user_id, retain = retain1 + retain2)

                       expansion <- alltable %>%
                         filter(   (!is.na(MRR_current)) &  MRR_current > 0 ) %>%
                         filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 )%>%
                         filter(MRR_current > MRR_last_month) %>%
                         impute0()%>%
                         transmute(user_id = user_id, expansion = MRR_current-MRR_last_month)

                       contraction <- alltable %>%
                         filter( (!is.na(MRR_current)) &  MRR_current > 0  )%>%
                         filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 ) %>%
                         filter(MRR_current < MRR_last_month) %>%
                         impute0()%>%
                         transmute(user_id = user_id, contraction =MRR_last_month-MRR_current)

                       churn <- alltable %>%
                         filter( is.na(MRR_current) | MRR_current <= 0 ) %>%
                         filter( (!is.na(MRR_last_month)) & MRR_last_month > 0 ) %>%
                         transmute(user_id = user_id, churn =MRR_last_month)

                       res0 = merge(new, resurrected, all = T)
                       res0 = merge(res0, retain, all = T)
                       res0 = merge(res0, expansion, all = T)
                       res0 = merge(res0, contraction, all = T)
                       res0 = merge(res0, churn, all = T)
                       res0 = impute0(res0)

                       res0$date = time_point
                       res0 = res0 %>% filter( !(new == 0 &  resurrected == 0 & retain == 0 &  expansion == 0 & contraction == 0 & churn == 0) )

                       return(res0)
                     }
                     ) )

  return(res_test)
}
