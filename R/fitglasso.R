#' Fit roup lasso logistic regression
#' @description Fit roup lasso logistic regression and returns a list object
#' @param trainx  a data frame where samples are in rows and features are in columns
#' @param trainy a numeric or factor vector containing the outcome for each sample
#' @param lambda  value of tuning parameter lambda
#'
#' @return
#' A \code{grplasso} object is returned, for which \code{coef}, \code{print}, \code{plot} and \code{predict} methods exist.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' data("sim1_da1")
#' trainx = dplyr::select(sim1_da1, -y)
#' trainy = sim1_da1$y
#' # index of the group
#' index <- gsub("\\..*", "", names(trainx))
#' # nlam is the number of values of tuning variable
#' nlam <- 20
#' # type of prediction
#' type = "link"
#' # number of cross-validation folds
#' kfold <- 10
#' cv_fit <- cv_glasso(trainx, trainy, nlam = nlam, kfold = kfold)
#' str(cv_fit)
#'
#' fitgl <- fitglasso(trainx = trainx, trainy= trainy, lambda = cv_fit$lambda.max.auc[1])
#' }
#' @export
#'

fitglasso <- function(trainx, trainy, lambda, na_action = na.pass) {
  library(grplasso)
  n <- nrow(trainx)
  # Do grouplasso logistic regression
  x <- cbind(1, as.matrix(trainx))
  colnames(x) <- c("Intercept", colnames(trainx))
  index <- c(NA, as.factor(index))
  ################################# Get a vector of tuning parameters
  fit <- grplasso(x, trainy, index = index, lambda = lambda)
  # class(fit)<- "fitglasso"
  return(fit)
}
