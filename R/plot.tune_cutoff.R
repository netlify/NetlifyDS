#' Plot cutoff tuning process from "\code{tune_cutoff}"
#' @description Plots cutoff tuning process
#' @param x "\code{tune_cutoff}" object
#' @param pch type of cutoff points, default is 20
#' @param cex size of cutoff points, default is 1.5, col = "red"
#' @param ... other parameters in "\code{plot()}" function
#' @param col color of cutoff points, default is \code{col = "red"}
#' @author Hui Lin, \email{longqiman@gmail.com}
#'
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
plot.tune.cutoff <- function (x, pch= 20, cex = 1.5, col = "red", ...) {
  dat <- x$cutoff.tune
  plot(dat$PPV, dat$NPV, type = "l", xlab = "True Positive", ylab = "True Negative",
       main = paste("Likelihood groups:", paste(x$likelihood, collapse = ", ") ), ...)
  points(dat$PPV[x$cutoffs], dat$NPV[x$cutoffs], pch = pch, cex = cex, col =  col)
  text(dat$PPV[x$cutoffs], dat$NPV[x$cutoffs]+0.1, x$cutoffs)
}

