% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypower.R
\name{mypower}
\alias{mypower}
\title{mypower}
\usage{
mypower(mu0 = 0, sigma, n, delta, alpha = 0.05)
}
\arguments{
\item{mu0}{mu_0 from H_0}

\item{sigma}{standard deviation (known)}

\item{n}{sample size}

\item{delta}{difference between the means}

\item{alpha}{alpha level}
}
\value{
a power plot of power vs n and a named list containing power, beta, sigma, delta, and alpha
}
\description{
A function that calculates power and beta
}
\examples{
\dontrun{mypower(mu0 = 10, sigma = 2,n = 10,delta = 2)}
}
