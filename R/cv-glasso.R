#' Cross-validation for group lasso logistic regression
#' @description Does k-fold cross-validation for group lasso logistic regression and returns a list object
#' @param trainx  a data frame where samples are in rows and features are in columns
#' @param trainy a numeric or factor vector containing the outcome for each sample
#' @param nlam    number of lambda values. The default is 100
#' @param type  the type of prediction. \code{type = 'link'} is on the scale of linear predictors (default), whereas \code{type = 'response'} is on the scale of the response  variable, i.e. \code{type = 'response'} applies the inverse link function to the linear predictors.
#' @param na_action function determining what should be done with missing values when predicting new data during cross validation. The default is to predict NA.
#' @param kfold number of folds - default is 10. Although nfolds can be as large as the sample size (leave-one-out CV), it is not recommended for large datasets. The default is 10.
#' @return
#' an object of class "\code{cv_glasso}" is returned, which is a list with the ingredients of the cross-validation fit.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @examples
#' \dontrun{
#' data("sim1_da1")
#' trainx = dplyr::select(sim1_da1, -y)
#' trainy = sim1_da1$y
#' # index of the group
#' index <- gsub("\\..*", "", names(trainx))
#' # nlam is the number of values of tuning variable
#' nlam <- 10
#' # type of prediction
#' type = "link"
#' # number of cross-validation folds
#' kfold <- 10
#' cv_fit <- cv_glasso(trainx, trainy, nlam = nlam, kfold = kfold)
#' str(cv_fit)
#' }

#' @export
#'
cv_glasso <- function(trainx, trainy, nlam = 100, type = "link", kfold = 10,
                      na_action = na.pass) {

  library(grplasso)
  library(caret)
  library(pROC)
  n <- nrow(trainx)
  # Do grouplasso logistic regression
  x <- cbind(1, as.matrix(trainx))
  colnames(x) <- c("Intercept", colnames(trainx))
  index <- c(NA, as.factor(index))
  ################################# Get a vector of tuning parameters
  lambda <- lambdamax(x, y = trainy, index = index, penscale = sqrt,
                      model = LogReg()) * 0.96^(1:nlam)

  index0 <- c(1:n)
  y0 <- NULL
  pre <- NULL

  for (i in 1:kfold) {
    idx <- sample(index0, round(n/kfold, 0), replace = F)
    test <- trainx[idx, ]
    train <- trainx[-idx, ]
    y0 <- c(y0, trainy[idx])
    xtest <- cbind(1, as.matrix(test))
    xtrain <- cbind(1, as.matrix(train))
    colnames(xtrain) <- c("Intercept", colnames(train))
    colnames(xtest) <- c("Intercept", colnames(test))
    fit <- grplasso(xtrain, trainy[-idx], index = index, lambda = lambda,
                    model = LogReg(), penscale = sqrt)
    pre0 <- predict(fit, xtest, type = type, na.action = na_action)
    pre0 <- data.frame(pre0)
    pre <- rbind(pre, pre0)
    index0 <- index0[-idx]
  }

  auc <- rep(0, ncol(pre))
  for (i in 1:ncol(pre)) {
    auc[i] <- as.numeric(auc(y0, pre[, i]))
  }
  pmsure = auc
  lambda.max.auc = c(lambda = lambda[which(pmsure == max(pmsure))], auc = max(pmsure))
  se.auc = max(pmsure) - sd(pmsure)
  lambda.1se.auc = c(lambda[min(which(pmsure >= se.auc))], se.auc = se.auc)
  ############################## Maximize log-likelihood
  loglike <- rep(0, ncol(pre))
  for (i in 1:ncol(pre)) {
    loglike[i] <- sum((pre[, i] - log(1 + exp(pre[, i]))) * y0 + (-log(1 +
                                                                         exp(pre[, i]))) * (1 - y0))
  }
  pmsure = loglike
  lambda.max.loglike = c(lambda = lambda[which(pmsure == max(pmsure))], loglike = max(pmsure))
  se.loglike = max(pmsure) - sd(pmsure)
  lambda.1se.loglike = c(lambda = lambda[min(which(pmsure >= se.loglike))], se.loglike = se.loglike)
  ############################## Maximize correlation
  co <- pre
  maxco <- rep(0, ncol(pre))
  for (i in 1:ncol(pre)) {
    s <- sort(pre[, i])
    for (j in 1:nrow(pre)) {
      yhat <- as.numeric(pre[, i] >= s[j])
      co[j, i] <- cor(yhat, y0)
    }
    maxco[i] <- max(na.omit(co[, i]))
  }
  pmsure = maxco
  lambda.max.maxco = c(lambda = lambda[which(pmsure == max(pmsure))], maxco = max(pmsure))
  se.maxco = max(pmsure) - sd(pmsure)
  lambda.1se.maxco = c(lambda = lambda[min(which(pmsure >= se.maxco))], se.maxco = se.maxco)
  ###############
  res <- list(lambda = lambda, pred = pre, auc = auc, log_likelihood = loglike,
              maxrho = maxco, lambda.max.auc = lambda.max.auc, lambda.1se.auc = lambda.1se.auc,
              lambda.max.loglike = lambda.max.loglike, lambda.1se.loglike = lambda.1se.loglike,
              lambda.max.maxco = lambda.max.maxco, lambda.1se.maxco = lambda.1se.maxco)
  class(res) <- "cv_glasso"
  return(res)
  invisible(res)
}
