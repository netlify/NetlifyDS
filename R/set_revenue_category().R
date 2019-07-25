# don't export this function
# it is used to set revenue category
set_revenue_category <- function () {
  assign("team", c("netlify_team_premium_monthly","Teams","netlify_team_plus_monthly","netlify_team_plus_yearly"), envir = .GlobalEnv)
 
  assign("legacy", c("domain", "site", "ssl", "bitballoon-monthly-june-2013", "Legacy Netlify Sites",
                   "bitballoon-yearly-june-2013", "automation"), envir = .GlobalEnv)
  assign("inside", c("Enterprise plans", "Reseller plans","Unlimited-Identity"), envir = .GlobalEnv)
  assign("add_ons", c("Forms", "Functions", "Identity", "Domains","Large Media", "Analytics"), envir = .GlobalEnv)
  assign("core", c(team, "Extra team members"), envir = .GlobalEnv)
  assign("selfServe", c(core, legacy, add_ons), envir = .GlobalEnv)
}
