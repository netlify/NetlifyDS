## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE2 <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE,pce_less_than_zero=FALSE) {

  require(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  qx<-function(x,q){

  sort( na.omit(x) )[round(length( na.omit(x) )*q,0)]->res
    return(res)
  }

  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 if (pce_less_than_zero)
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      pct = mean   (xx[,col]>0, na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                      #q25   = qx(xx[,col],0.25) ,
                      #q75   = qx(xx[,col],0.75) ,
                      #q5   = qx(xx[,col],0.05) ,
                      #q95   = qx(xx[,col],0.95) ,
                      #q15   = qx(xx[,col],0.15) ,
                      #q85   = qx(xx[,col],0.85) ,

                   )
                 }
                 else
                   .fun= function(xx, col, na.rm) {
                     c( N    = length2(xx[,col], na.rm=na.rm),
                        mean = mean   (xx[,col], na.rm=na.rm),
                        pct = mean   (xx[,col]>0, na.rm=na.rm),
                        sd   = sd     (xx[,col], na.rm=na.rm)
                       # q5   = qx(xx[,col],0.05) ,
                      #  q15   = qx(xx[,col],0.15) ,
                      #  q25   = qx(xx[,col],0.25) ,
                      #  q75   = qx(xx[,col],0.75) ,
                      #  q85   = qx(xx[,col],0.85) ,
                      #  q95   = qx(xx[,col],0.95)

                     )
                   },
                 measurevar,
                 na.rm
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean"=measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
