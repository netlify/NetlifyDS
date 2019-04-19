#' Separate data frame according to variable type
#'
#' @param dat a data frame.
#'
#' @export
sep_data <- function(dat) {
  ######################################## separate predictors according to their attribute
  id_opstus <- grep("OPSTATUSCD", names(dat))
  id_cate <- which(names(dat) %in% c("OPER_ID", "BU_NM", "CU_NM", "CU_LD_NM",
                                     "SLS_AREA_CD", "SLS_TERR_CD", "BP_PRIMDEC_STATE"))
  ##----------------------------------------
  id_ind <- grep("IND", names(dat))
  ##----------------------------------------
  id_disc <- grep("DISC_AMT", names(dat))
  id_pctdp <- grep("DP_pct", names(dat))
  id_conc <- grep("CONC", names(dat))
  ##----------------------------------------
  id_rev <- c(grep("REV_AMT", names(dat)), grep("PTNTL_REV", names(dat)),
              grep("NET_SLS_AMT", names(dat)))
  id_payment <- c(grep("CC20", names(dat)), grep("DP20", names(dat)),
                  grep("Ach", names(dat)), grep("Cash", names(dat)), grep("TOT_PAY",
                                                                          names(dat)))
  ##----------------------------------------
  id_acre <- grep("COMM_ACR_CNT", names(dat))
  id_ptnl <- grep("PTNTL_UNIT", names(dat))
  ##----------------------------------------
  id_samunits <- c(grep("SamUnits", names(dat)), grep("CumSumUnits",
                                                      names(dat)))
  # id_regsam<-grep('REG_SAM',names(dat))
  id_samcorn <- grep("SAM_CORN20", names(dat))
  id_samsoy <- grep("SAM_SOY20", names(dat))
  id_samcanola <- grep("SAM_CANOLA20", names(dat))
  ##----------------------------------------
  id_prodct <- grep("PROD_CT", names(dat))
  ##----------------------------------------
  id_fit <- grep("FITSERV", names(dat))
  id_perf <- grep("PERF", names(dat))
  ################################################################
  datcate <- dat[, id_cate]
  datind <- dat[, id_ind]
  datdisc <- dat[, id_disc]
  datpct <- dat[, c(id_pctdp, id_conc)]
  datdollar <- dat[, c(id_rev, id_payment)]
  datacre <- dat[, c(id_acre, id_ptnl)]
  datunit <- dat[, c(id_samunits, id_samcorn, id_samsoy, id_samcanola)]
  datct <- dat[, id_prodct]
  datperf <- dat[, id_perf]
  datfit <- dat[, id_fit]
}
