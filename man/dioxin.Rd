\name{dioxin}
\alias{dioxin}
\docType{data}
\title{Average Daily Ingestion of Dioxin in Food Samples with Uncertainities

}
\description{
This dataset contains the estimated average daily ingestion of dioxins from food samples collected across Japan, including uncertainties in the values. Dioxins are toxic chemical compounds that pose significant health risks.
}
\usage{data("dioxin")}
\format{
 The format is:
List of 17 numeric interval values
}
\details{
This data provides an analysis of dioxin intake and its potential health impacts including exposure levels from various food sources in Japan.
}

\source{
The dataset was collected and monitored by the Ministry of Environment, Japan, as reported in their environmental statistics
}
\references{
Zahid Khan, Mohammed M. A. Almazah, Omalsad Hamood Odhah, and Huda M. Alshanbari (2022): Generalized Pareto Model: Properties and Applications in Neutrosophic Data Modeling. Mathematical Problems in Engineering, 2022(1). doi:10.1155/2022/3686968
}
\examples{
data(dioxin)
 # Provide neutrosophic summary statistics
nsummary(dioxin)

}
\keyword{datasets}
