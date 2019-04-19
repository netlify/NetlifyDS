#' Identify outliers using MAD
#'
#' @param x a vector
#' @return a vector indicating identified outliers
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' x<-c(seq(1:1000),20000)
#' out_mad(x)
#' }

#' @export
########################outliers
out_mad<-function(x){
v2<-x-median(na.omit(x))
mad<-median(na.omit(abs(v2)))
idx<-which(0.6745*(x-median(na.omit(v2)))/mad>3.5)
return(idx)
}

