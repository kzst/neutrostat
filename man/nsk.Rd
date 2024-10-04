\name{nsk}
\alias{nsk}
\title{ Neutrosophic Pearson Coefficient of Skewness}
\description{Neutrosophic skewness  is imprecise value that measures the asymmetery of neutrosophic data using the method of moments}
\usage{
nsk(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
An interval value of Pearson coefficeint of skewness.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
Aslam, Muhammad (2021): A study on skewness and kurtosis estimators of wind speed distribution under indeterminacy, Theoretical and Applied Climatology, 143(3), pp. 1227-1234.doi:10.1007/s00704-020-03509-5
}
\author{
Zahid Khan
}
\seealso{
\code{\link{nmean}},\code{\link{nstd}}.
}
\examples{
data <- list(c(1, 2), c(4), c(2, 3),c(6,8),c(12,20))
s <- nsk(data)
print(s)
}
