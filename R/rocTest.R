#' Compute the area under the ROC curve and compare different AUC's
#' @description This function compares the AUC of two correlated (or paired) or uncorrelated (un- paired) ROC curves.
#' @author Hui Lin, \email{longqiman@gmail.com}
#' @param y  response
#' @param x  prediction
#' @param L  list to assign the comparisons

#' @export
rocTest <- function(y, x, L = NULL) {

    trapezarea <- function (x, y)
    {
        if (x[1] > x[length(x)]) {
            x <- rev(x)
            y <- rev(y)
        }
        if (length(x) != length(y))
            stop("length x must equal length y")
        if (length(unique(x)) < 2)
            return(NA)
        ya <- approx(x, y, 0, ties = max, rule = 2)$y
        yb <- approx(x, y, 1, ties = max, rule = 2)$y
        x <- c(0, x, 1)
        y <- c(ya, y, yb)
        h <- diff(x)
        lx <- length(x)
        area <- 0.5 * sum(h * (y[-1] + y[-lx]))
        area
    }

    th <- NULL
    sens <- spec <- list(rep(NULL, length(x)))
    for (j in 1:length(x)) {
        DD <- table(-x[[j]], y)
        sens[[j]] <- c(0, cumsum(DD[,2])/sum(DD[,2]))
        spec[[j]] <- c(1, 1 - cumsum(DD[,1])/sum(DD[,1]))
        th[j] <- trapezarea(1 - spec[[j]], sens[[j]])
    }

    if (!is.null(names(x))) {
        names(sens) <- names(x)
        names(spec) <- names(x)
    }
    else {
        names(sens) <- paste("Test", LETTERS[1:length(x)])
        names(spec) <- paste("Test", LETTERS[1:length(x)])
    }

    if (!is.null(L)) {
        V10 <- matrix(NA, nrow = length(y[y == 1]), ncol = length(x))
        V01 <- matrix(NA, nrow = length(y[y == 0]), ncol = length(x))

        for (j in 1:length(x)) {
            x.s <- split(x[[j]], y)
            for (i in 1:length(x.s$"1"))
                V10[i, j] <- (length(x.s$"0"[x.s$"0" < x.s$"1"[i]]) + .5 * length(x.s$"0"[x.s$"0" == x.s$"1"[i]])) / length(y[y == 0])
            for (i in 1:length(x.s$"0"))
                V01[i, j] <- (length(x.s$"1"[x.s$"0"[i] < x.s$"1"]) + .5 * length(x.s$"1"[x.s$"1" == x.s$"0"[i]])) / length(y[y == 1])
        }

        S10 <- (t(V10) %*% V10 - length(y[y == 1]) * th %*% t(th)) / (length(y[y == 1]) - 1)
        S01 <- (t(V01) %*% V01 - length(y[y == 0]) * th %*% t(th)) / (length(y[y == 0]) - 1)

        S <- S10 / length(y[y == 1]) + S01 / length(y[y == 0])

        contr <- L %*% th
        se <- sqrt((L %*% S %*% t(L)))

        test <- t(th) %*% t(L) %*% solve(L %*% (1 /length(y[y ==1]) * S10 + 1 / length(y[y ==0]) * S01) %*% t(L), t(t(th) %*% t(L)))

        p.value <- pchisq(test, df = qr(L %*% S %*% t(L))$rank, lower.tail = FALSE)
    }
    else {
        S <- NULL
        p.value <- NULL
        contr <- NULL
    }

    names(th) <- names(x)
    res <- list(th = th, sens = sens, spec = spec, contr = contr, S = S, p.value = p.value)
    class(res) <- "rocTest"
    invisible(res)
}
