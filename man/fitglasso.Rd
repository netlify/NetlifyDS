% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitglasso.R
\name{fitglasso}
\alias{fitglasso}
\title{Fit roup lasso logistic regression}
\usage{
fitglasso(trainx, trainy, lambda, na_action = na.pass)
}
\arguments{
\item{trainx}{a data frame where samples are in rows and features are in columns}

\item{trainy}{a numeric or factor vector containing the outcome for each sample}

\item{lambda}{value of tuning parameter lambda}
}
\value{
A \code{grplasso} object is returned, for which \code{coef}, \code{print}, \code{plot} and \code{predict} methods exist.
}
\description{
Fit roup lasso logistic regression and returns a list object
}
\examples{
\dontrun{
data("sim1_da1")
trainx = dplyr::select(sim1_da1, -y)
trainy = sim1_da1$y
# index of the group
index <- gsub("\\\\..*", "", names(trainx))
# nlam is the number of values of tuning variable
nlam <- 20
# type of prediction
type = "link"
# number of cross-validation folds
kfold <- 10
cv_fit <- cv_glasso(trainx, trainy, nlam = nlam, kfold = kfold)
str(cv_fit)

fitgl <- fitglasso(trainx = trainx, trainy= trainy, lambda = cv_fit$lambda.max.auc[1])
}
}
\author{
Hui Lin, \email{longqiman@gmail.com}
}
