#' Calculate revenue accounting matrix per day, user and product
#' @description It calculates the accounting matrix for each user and product given date, subscription data and product category.
#' @param dates  a date vector (have to be date type)
#' @param dates_full  a full range of dates
#' @param datall_revenue daily revenue data, need to have columns:  user_id, Product, active_date, MRR
#' @param type a vector of product names
#' @return A revenue accounting matrix is returned for each day, user and product
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' team <- c("netlify_team_premium_monthly","Teams","netlify_team_plus_monthly","netlify_team_plus_yearly")
#' res_0 = RevActUserProduct(total_revenue, dates, team)
#' }
#' @export
#'

RevActUserProduct <- function(datall_revenue, dates, dates_full, type){

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

  ######################## BEGIN OF TEM_FUN #############################
  tem_fun = function(time_point, dat, dates_full){

    cmon = time_point

    # the previous month
    pmon = floor_date(time_point, unit = "month")- days(1)

    # the legacy month
    lmon = dates_full[dates_full < pmon]

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
      select(user_id, new = cmrr )

    # Get reesurrected
    resurrected <- alltable %>%
      filter(cmrr > 0 & pmrr == 0 & lmrr > 0) %>%
      select(user_id,resurrected = cmrr)

    # Get Retain
    retain1 <- alltable %>%
      filter(cmrr > 0 & pmrr > 0) %>%
      filter(cmrr >= pmrr) %>%
      select(user_id,retain1 = pmrr)

    retain2 <- alltable %>%
      filter(cmrr > 0 & pmrr > 0) %>%
      filter(cmrr < pmrr) %>%
      select(user_id,retain2 = cmrr)

    retain = merge(retain1, retain2, all=T) %>%
      NetlifyDS::impute_dat(method = "zero")%>%
      transmute(user_id = user_id, retain = retain1 + retain2)

    # Get expansion
    expansion <- alltable %>%
      filter(cmrr > 0 & pmrr >0) %>%
      filter(cmrr > pmrr) %>%
      transmute(user_id = user_id, expansion = cmrr -pmrr)

    # Get contraction
    contraction <- alltable %>%
      filter(cmrr > 0 & pmrr >0) %>%
      filter(cmrr < pmrr) %>%
      transmute(user_id = user_id, contraction =pmrr-cmrr)

    # Get churn
    churn <- alltable %>%
      filter(cmrr == 0 & pmrr > 0) %>%
      transmute(user_id = user_id, churn =pmrr)

    res0 = merge(new, resurrected, all = T)
    res0 = merge(res0, retain, all = T)
    res0 = merge(res0, expansion, all = T)
    res0 = merge(res0, contraction, all = T)
    res0 = merge(res0, churn, all = T)
    res0 = NetlifyDS::impute_dat(res0, method = "zero")

    res0$date = time_point
    return(res0)
  }

  ######################## END OF TEM_FUN #############################

  res = do.call("rbind", lapply(dates, function(time_point){tem_fun(time_point, dat = dat, dates_full = dates_full)}))
  res = res %>% filter( !(new == 0 &  resurrected == 0 & retain == 0 &  expansion == 0 & contraction == 0 & churn == 0) )

  return(res)
}
