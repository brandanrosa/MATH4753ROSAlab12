#' mylam
#'
#' This function displays histograms of the log(lambda) and ybar statistics from a given sample
#'
#' @param iter number of iterations
#' @param y sample data
#' @param alpha alpha level
#'
#' @return two histograms and other information which can be called
#' @export
#'
#' @examples \dontrun{mylam(iter=10000,y=y,alpha=0.05)}
mylam <- function(iter, y, alpha = 0.05) {
  n <- length(y)

  sam <- sample(x = y, size = n*iter, replace = TRUE)
  mat <- matrix(sam, nrow = n, ncol = iter)

  loglam <- function(x) {
    n1 <- length(x)
    (-n1 * mean(x)^2) / 2
  }

  meany <- function(x) {
    mean(x)
  }

  xstat1 <- apply(X = mat, MARGIN = 2, FUN = "loglam")
  h1 <- hist(xstat1, plot = FALSE)
  r1 <- h1$density/max(h1$density)

  hist1 <- hist(x = xstat1,
                freq = FALSE,
                col = rgb(r1, r1^2, 0),
                xlab = "log(lambda)",
                main = "Sampling Distribution of log(lambda)",
  )
  hist1

  loglamcut <- quantile(xstat1, probs = alpha)
  abline(v = loglamcut, col = 'blue', lwd=2)

  xstat2 <- apply(X = mat, MARGIN = 2, FUN = "meany")
  h2 <- hist(xstat2, plot = FALSE)
  r2 <- h2$density/max(h2$density)

  hist2 <- hist(x = xstat2,
                freq = FALSE,
                col = rgb(r2, r2^2, 0),
                xlab = "Ybar",
                main = "Sampling Distribution of Ybar",
  )
  hist2

  ybarcut <- quantile(x = xstat2, probs = 1-alpha)
  abline(v = ybarcut, col = 'blue', lwd=2)

  ybarcutest <- sqrt((-2 * loglamcut)/n)

  l <- list(xstat1=xstat1,
            xstat2=xstat2,
            loglamcut=loglamcut,
            ybarcut=ybarcut,
            sam=sam,
            mat=mat,
            ybarcutest=ybarcutest
  )

  invisible(l)
}
