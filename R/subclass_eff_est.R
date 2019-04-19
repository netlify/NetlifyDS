#' Estimation ATT and ATE after stratification
#' @description After stratification, estimation the weighted mean difference. Return estimates of ATT and ATE
#' @param object  The output object from matchit. This is a required input.
#' @param y outcome variable. This is a required input.
#' @return
#' Fuction returns a list with the ATT and ATE estimates
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' subclass_eff_est(m2.out, match.data(m2.out)$CORN_UNITS_CHG)
#' }

#' @export
#'
#'
subclass_eff_est <- function(object, y){

mdat_str = tibble(subclass = object$subclass,
       treatment = object$treat,
       y = y
)


TAOs = mdat_str%>%
  group_by(subclass,treatment)%>%
  summarise(ybar = mean(y))%>%
  ungroup()%>%
  group_by(subclass)%>%
  summarise(taos=diff(ybar))

Ns <- mdat_str%>%
  group_by(subclass)%>%
  summarise(Ns = n(), Ns1 = sum(treatment))%>%
  mutate(wt_ate = Ns/sum(Ns), wt_att = Ns1/sum(mdat_str$treatment))

tao_att = sum(TAOs$taos * Ns$wt_att)
tao_ate = sum(TAOs$taos * Ns$wt_ate)

###############
res <- list(tao_att = tao_att,
            tao_ate = tao_ate)
return(res)
invisible(res)
}
