\name{goldprice}
\alias{goldprice}
\docType{data}
\title{Gold Prices Across Six Indian Cities from February 2022 to January 2023
}

\description{
The dataset provides the monthly high and low prices (in rupees per gram) of 22-carat gold in six Indian cities: Chennai, Kolkatta,Bangal,  .Data were collected from February 2022 to January 2023. This data can be used for neutrosophic statistical analysis of gold price trends.
}
\usage{data("goldprice")}
\format{
  A data frame with 12 observations on the following 13 variables.
  \describe{
    \item{\code{Month}}{a character vector}
    \item{\code{Chennai_Low}}{a numeric vector}
    \item{\code{Chennai_High}}{a numeric vector}
    \item{\code{Kolkatta_Low}}{a numeric vector}
    \item{\code{Kolkatta_High}}{a numeric vector}
    \item{\code{Bangalore_Low}}{a numeric vector}
    \item{\code{Bangalore_High}}{a numeric vector}
    \item{\code{Madurai_Low}}{a numeric vector}
    \item{\code{Madurai_High}}{a numeric vector}
    \item{\code{Hyderabad_Low}}{a numeric vector}
    \item{\code{Hyderabad_High}}{a numeric vector}
    \item{\code{Delhi_Low}}{a numeric vector}
    \item{\code{Delhi_High}}{a numeric vector}
  }
}
\details{
Monthly high and low gold prices in Chennai, Kolkatta, and Bangalore. These can be analyzed using neutrosophic statistical methods to evaluate variations and trends.
}
\source{
Indian Daily Gold Prices Android App
}
\references{
 Kala Raja Mohan, R. Narmada Devi, Nagadevi Bala Nagaram, T. Bharathi, and Suresh Rasappan (2023): Neutrosophic Statistical Analysis on Gold Rate. Neutrosophic Sets and Systems, 60(1). doi:10.5281/zenodo.7535991
}
\examples{
#list of low and high gold price for Chennai City
ch<- mapply(function(low, high) list(c(low, high)),
                                       goldprice$Chennai_Low,
                                       goldprice$Chennai_High)

# neutrosophic coefficient of variation
ncv(ch)
}
\keyword{datasets}
