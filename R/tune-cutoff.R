#' Tune the cutoffs for different likelihood groups
#' @description Tune the cutoffs for different likelihood groups given predicted score, response and a vector of likelihood values
#' @param test a vector of predicted score from model
#' @param  y a vector of response, \code{y} need to be the same length of \code{test}
#' @param  likelihood a vector of likelihood values
#'
#' @return
#' an object of class "\code{tune_cutoff}" is returned, which is a list with the ingredients of the tuning process.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' data("sim1_da1")
#' trainx = sim1_da1[,1:50]
#' trainy = sim1_da1$y
#' library(glmnet)
#' fit <- cv.glmnet(as.matrix(trainx), trainy, family = "binomial")
#' test <- predict(fit, as.matrix(trainx), type = "link", s = "lambda.min")
#' test <- as.vector(test)
#' summary(test)
#' likelihood <- c(0.2, 0.5, 0.8)
#' y <- trainy
#' x <- tune_cutoff(test = test, y = y, likelihood = likelihood)
#' str(x)
#' plot.tune.cutoff(x)
#' }
#'
#' @export
#'
tune_cutoff <- function(test, y, likelihood) {

  n <- length(y)
  ppv <- rep(-1, n)
  npv <- rep(-1, n)
  for (i in 1:n) {
    t <- sort(test)[i]
    as.numeric(test >= t)
    np <- sum(test >= t)
    nn <- n - np
    tp <- sum((test >= t) * y)
    tn <- sum((test < t) * (1 - y))
    ppv[i] <- tp/np
    npv[i] <- tn/nn
  }

  ta <- data.frame(PPV = ppv, NPV = npv, cutoff = sort(test))
  ResPre <- data.frame(cbind(cutoff = test, testClasses = y))
  ResPre <- ResPre[order(ResPre$cutoff), ]
  ta$testClasses <- ResPre$testClasses
  # ------------------------------------------------------------------
  # tune the cutoffs
  likelihood <- sort(likelihood)

  rk <- seq_along(likelihood)
  rk <- c(1, length(rk), rk[2:(length(rk) - 1)])
  likelihood <- likelihood[rk]

  ids <- seq(1:(length(likelihood) - 1))

  for (i in seq_along(ids)) {
    i=2
    p <- likelihood[i]

    id1s <- NULL

    ##------------------ the 1st group
    if (i == 1) {
      for (j in seq_along(ta$testClasses)) {
        if (mean(ta$testClasses[1:j], na.rm = T) <= p) {
          id1s <- c(id1s, j)
        }
      }
      ids[i] <- max(id1s, na.rm = T)
    }

    ##------------------ the 2nd group
    if (i == 2) {
      for (j in ids[1]:length(ta$testClasses)) {
        if (mean(ta$testClasses[j:length(ta$testClasses)], na.rm = T) >= p) {
          id1s <- c(id1s, j)
        }
      }
      ids[i] <- min(id1s, na.rm = T)
    }

    ##------------------ the 3rd group
    if (i == 3) {
      for (j in (ids[1] + 1):(ids[2] - 1)) {
        if (mean(ta$testClasses[(ids[1] + 1):j], na.rm = T) <= p) {
          id1s <- c(id1s, j)
        }
      }
      ids[i] <- max(id1s, na.rm = T)
    }
    ##------------------ the rest groups
    if (i > 3) {
      for (j in (ids[i - 1] + 1):(ids[2] - 1)) {
        if (mean(ta$testClasses[(ids[i - 1] + 1):j], na.rm = T) <= p) {
          id1s <- c(id1s, j)
        }
      }
      ids[i] <- max(id1s, na.rm = T)
    }
  }
  res <- list(cutoff.tune = ta,
  cutoffs = sort(ids),
  likelihood = sort(likelihood)
  )
  
  class(res) <- "tune_cutoff"
  return(res)
  invisible(res)
}


