#' Plot ROC curve
#' @description Plot ROC curve based on rocTest object
#' @param x  rocTest object
#' @author Hui Lin, \email{longqiman@gmail.com}

#' @export
plot.rocTest <- function(x, auto.legend = TRUE, ...)

{

    plot(c(0,1), c(0,1), type = "n",ann=FALSE, asp=0,xaxs='i',yaxs='i',xaxt='n',yaxt='n')

axis(1,c(0,.5,1))

axis(2,c(0,.5,1))

    for (j in 1:length(x$sens)) {

        lines(I(1 - x$spec[[j]]), x$sens[[j]], lty = j)

        abline(0, 1, lty = 6)

    }

    if (auto.legend) legend(0.6,0.3,
    #"bottomright",
    lty = c(1:length(x$sens),6),

    #col = 1:length(x$sens),

     c('Group Lasso','Expert Opinion','Diagonal'), bty = "n",cex=1

# xjust = 1, yjust = 0

)

}
