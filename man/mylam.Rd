% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mylam.R
\name{mylam}
\alias{mylam}
\title{mylam}
\usage{
mylam(iter, y, alpha = 0.05)
}
\arguments{
\item{iter}{number of iterations}

\item{y}{sample data}

\item{alpha}{alpha level}
}
\value{
two histograms and other information which can be called
}
\description{
This function displays histograms of the log(lambda) and ybar statistics from a given sample
}
\examples{
\dontrun{mylam(iter=10000,y=y,alpha=0.05)}
}
