#' Impute missing values
#'
#' @param dat a data frame.
#' @param method imputation method. Need to be one of these: \code{zero}, \code{mean}, \code{Inf0}, \code{code99}
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' library(DataScienceR)
#' data("SegData")
#' impdat <- SegData[, c("income", "house")]
#' # summary(SegData)
#' impdat = impute_dat(impdat, method = "mean")
#' summary(impdat)
#' }

#' @export
impute_dat <- function (dat, method){
  ##--------------------------------- zero
  if (method == "zero")
  {
    for (i in 1:ncol(dat)) {
      idx <- which(is.na(dat[, i]))
      if (length(idx))
        dat[, i][idx] <- 0
    }
    return(dat)
  }
  ##--------------------------------- mean
  else if (method == "mean")
  {
    for (i in 1:ncol(dat)) {
      idx <- which(is.na(dat[, i]))
      if (length(idx))
        dat[, i][idx] <- mean(na.omit(dat[, i]))
    }
    return(dat)
  }
  ##--------------------------------Impute Inf to be 0
  else if (method == "Inf0")
  {
    for (i in 1:ncol(dat)) {
      idx <- which(abs(dat[, i]) == Inf)
      if (length(idx))
        dat[, i][idx] <- 0
    }
    return(dat)
  }
  ##------------------------------Impute 99 to be 0
  else if (method == "code99")
  {
    for (i in 1:ncol(dat)) {
      idx <- which(dat[, i] == 99)
      if (length(idx))
        dat[, i][idx] <- 0
    }
    return(dat)
  }
  else
  {cat(" Error in argument \"method\", \n
       it has to be one of these: zero, mean, Inf0, code99")}
}


