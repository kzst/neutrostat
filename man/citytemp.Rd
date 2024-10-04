\name{citytemp}
\alias{citytemp}
\docType{data}
\title{Temperature Data of Five Different Cites in Pakistan for July 2022}
\description{
This dataset provides low and high recordings of daily temperature for five different citites (Gujranwala,Lahore,Islambad, Karachi and Sialkot ) of Pakistan for the  specifed priod July 2022
}
\usage{data("citytemp")}
\format{
  A data frame with 28 observations on the following 12 variables.
  \describe{
    \item{\code{Day}}{a character vector}
    \item{\code{Date}}{a numeric vector}
    \item{\code{Gujranwala_Low}}{a numeric vector}
    \item{\code{Gujranwala_High}}{a numeric vector}
    \item{\code{Lahore_Low}}{a numeric vector}
    \item{\code{Lahore_High}}{a numeric vector}
    \item{\code{Karachi_Low}}{a numeric vector}
    \item{\code{Karachi_High}}{a numeric vector}
    \item{\code{Islamabad_Low}}{a numeric vector}
    \item{\code{Islamabad_High}}{a numeric vector}
     \item{\code{Sialkot_Low}}{a numeric vector}
    \item{\code{Sialkot_High}}{a numeric vector}
  }
}
\details{
The data was collected for each city over 31 days in July 2022. It includes both the lower and upper temperature values, and can be analyzed using neutrosophic statistical approach.
}
\source{https://www.gismeteo.com/}
\references{
Ishmal Shahzadi (2023): Neutrosophic Statistical Analysis of Temperature of Different Cities of Pakistan. Neutrosophic Sets and Systems, 53(1). doi:10.5281/zenodo.7535991
}

\examples{
# list of temperature data for Gujranwala city
G <- mapply(function(low, high) list(c(low, high)),
                                       citytemp$Gujranwala_Low,
                                       citytemp$Gujranwala_High)
# Neutrosophic mean and standard deviation of temperature data for Gujranwala city
nmean(G)
nstd(G)
}
\keyword{datasets}
