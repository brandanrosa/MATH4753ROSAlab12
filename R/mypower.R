#' mypower
#'
#' A function that calculates power and beta
#'
#' @param mu0 mu_0 from H_0
#' @param sigma standard deviation (known)
#' @param n sample size
#' @param delta difference between the means
#' @param alpha alpha level
#'
#' @return a power plot of power vs n and a named list containing power, beta, sigma, delta, and alpha
#' @export
#'
#' @examples \dontrun{mypower(mu0 = 10, sigma = 2,n = 10,delta = 2)}
mypower <- function(mu0 = 0, sigma, n, delta, alpha=0.05) {
  moe <- qnorm(1-alpha/2)*sigma/sqrt(n)
  L <- mu0 - moe
  U <- mu0 + moe

  mu <- mu0 + delta
  zL <- (L-mu) / (sigma/sqrt(n))
  zU <- (U-mu) / (sigma/sqrt(n))

  beta <- pnorm(zU) - pnorm(zL)
  power <- 1 - beta

  # Power Plot
  x <- 0:30
  plotpwr <- pnorm(q = sqrt(x)-qnorm(1-alpha/2, 0 , 1), mean = 0, sd = 1)
  plot(x,
       plotpwr,
       type = "l",
       main = paste0("Power Calc: delta = ", delta, ", sd = ", sigma, ", alpha = ", alpha, sep=""),
       ylab = "Power",
       xlab = "n",
       col = "hotpink",
       lwd = 3
  )

  text(15, 0.4, paste0("H0: mu = ", mu0, sep=""))
  text(15, 0.3, paste0("H1: mu = ", mu, sep=""))
  mtext(paste0("Power = ", round(power,4), sep=""), side=3, col = "hotpink")

  list(power=power, beta=beta, n=n, delta=delta, alpha=alpha)
}
