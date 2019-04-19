#' Predict method for grplasso objects
#' @description Obtains predictions from a \code{grplasso} object.


#' @param object	a \code{grplasso} object
#' @param newdata	data.frame or design matrix of new observations
#' @param type	the type of prediction. \code{type = "link"} is on the scale of linear predictors, whereas \code{type = "response"} is on the scale of the response variable, i.e. \code{type = "response"} applies the inverse link function to the linear predictors.
#' na.action	function determining what should be done with missing values in newdata. The default is to predict NA.
#' @param ...	other options to be passed to the predict function.
#' @export
#'
predict_glasso <- function(object, newdata, type = c("link", "response"),
                              na.action = na.pass, ...) {
  library(grplasso)
  n <- nrow(newdata)
  # Do grouplasso logistic regression
  x <- cbind(1, as.matrix(newdata))
  return(predict(object, x, type = type, na.action = na.action, ...))
}
