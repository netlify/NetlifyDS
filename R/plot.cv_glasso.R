#' plot the cross-validation curve produced by  "\code{cv_glasso}"
#' @description Plots the cross-validation curve as a function of the lambda values used.
#' @param x  fitted "\code{cv_glasso}" object
#' @param type.measure criteria to use for cross-validation. Currently three options. The default is \code{type.measure = "auc"} which gives area under the ROC curve. \code{type.measure = "loglike"} computes the log-likelihood score in Meier et al2008. \code{type.measure = "maxco"} computes the maximum correlation coefficient in Yeo and Burge.
#' @return A plot is produced, and nothing is returned.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @references L. Meier, S. van de Geer, and P. Buhlmann, The group lasso for logistic regression, J. R. Stat. Soc. Ser. B Stat. Methodol. 70 (2008), pp. 53-71.
#' @references G.W. Yeo and C.B. Burge, Maximum entropy modeling of short sequence motifs with applications to RNA splicing signals, J. Computnl Biol. 11 (2004), pp. 475-494.
#'
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
#' cv.fit <- cv_glasso(trainx, trainy, nlam = nlam, kfold = kfold)
#' plot.cv_glasso(cv.fit)
#' }
#'
#' @export
plot.cv_glasso <- function (x, type.measure = "auc", ...)
{
  cvobj <- x
  xlab <- "Lambda"
  y <- cvobj[[type.measure]]
  plot.args = list(x = cvobj$lambda, y = y,
                   ylim = range(y)+c(-sd(y), sd(y)), xlab = xlab, ylab = type.measure,
                   type = "n")
  new.args = list(...)
  if (length(new.args))
    plot.args[names(new.args)] = new.args
  do.call("plot", plot.args)
 # error.bars(sign.lambda * log(cvobj$lambda), cvobj$cvup, cvobj$cvlo,
 #             width = 0.01, col = "darkgrey")
  points(cvobj$lambda, cvobj[[type.measure]], pch = 20,
         col = "red")
  # axis(side = 3, at = cvobj$lambda, labels = paste(cvobj$nz),
  #     tick = FALSE, line = 0)

  abline(v = cvobj[[paste("lambda.max",type.measure,sep=".")]][1], lty = 3)
  abline(v = cvobj[[paste("lambda.1se",type.measure,sep=".")]][1], lty = 3)
  invisible()
}


