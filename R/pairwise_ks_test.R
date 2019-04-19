#' Pairwise Kolmogorov-Smirnov Test
#' @description Perform Pairwise Multiple Comparisons Using Kolmogorov-Smirnov Test 
#' @param value a numeric vector of data values
#' @param group a group indicator
#' @param n_min  The minimum number of observations in each group. Group(s) with observation less than \code{n_min} will be removed. "\code{n_min=50}"(default)
#' @param warning sets the handling of warning messages. If \code{warning} is negative all warnings are ignored. If \code{warning} is zero (the default) warnings are stored until the top–level function returns. If 10 or fewer warnings were signalled they will be printed otherwise a message saying how many were signalled. An object called last.warning is created and can be printed through the function warnings. If \code{warning} is one, warnings are printed as they occur. If warn is two or larger all warnings are turned into errors.
#' @param alternative	indicates the alternative hypothesis and must be one of "\code{two.sided}" (default), "\code{less}", or "\code{greater}". You can specify just the initial letter of the value, but the argument name must be give in full. See ‘Details’ for the meanings of the possible values.
#' @details Missing values are silently omitted from x and (in the two-sample case) y.
#' 
#' The possible values "\code{two.sided}", "\code{less}" and "\code{greater}" of alternative specify the null hypothesis that the true distribution function of x is equal to, not less than or not greater than the hypothesized distribution function (one-sample case) or the distribution function of y (two-sample case), respectively. This is a comparison of cumulative distribution functions, and the test statistic is the maximum difference in value, with the statistic in the "greater" alternative being \eqn{D^+ = max[F_x(u) - F_y(u)]}. Thus in the two-sample case alternative = "greater" includes distributions for which x is stochastically smaller than y (the CDF of x lies above and hence to the left of that for y), in contrast to \code{t.test} or \code{wilcox.test}.
#' 
#' @return Pairwise Kolmogorov-Smirnov Test p-value Matrix
#' 
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' data("iris")
#' value<-iris$Sepal.Length
#' group<-iris$Species
#' pairwise_ks_test(value,group,warning = -1)
#' }
#' @export

pairwise_ks_test <- function(value, group, n_min = 50, warning = 0, alternative = "two.sided" ){
  
  lev <- unique(group)
  
  lst <- lapply( seq_along(lev), function(i) value[group == lev[i]] )
  names(lst)<-lev
  
  if (sum(lengths(lst)< n_min)) {
    lst <- lst [-which(lengths(lst)< n_min)]}
  
  f <- function(x, y){ 
    w <- getOption("warn") 
    options(warn = warning)  # ignore warnings 
    p <- ks.test(x, y, alternative = alternative, exact = 
                   F)$p.value 
    options(warn = w) 
    return(p) 
  } 
  
  res <- lapply(lst, function(x) lapply(lst, function(y) f(x, y))) 
  
  res<-unlist(res)
  res <- matrix(res, nrow = length(lst), ncol = length(lst), byrow = T)
  row.names(res) <- colnames(res) <- names(lst)
  cat("Pairwise Kolmogorov-Smirnov Test p-value Matrix","\n","\n")
  return(res)
}

