\name{nkur}
\alias{nkur}
\title{ Neutrosophic Coefficient of Kurtosis}
\description{Neutrosophic kurtosis  is an interval value that measures the flatness and peakedness  of neutrosophic data using the method of moments}
\usage{
nkur(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
An interval value of  coefficeint of Kurtosis.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749

Aslam, Muhammad (2021): A study on skewness and kurtosis estimators of wind speed distribution under indeterminacy, Theoretical and Applied Climatology, 143(3), pp. 1227-1234. doi:10.1007/s00704-020-03509-5
}
\author{
Zahid Khan
}
\seealso{
\code{\link{nsk}}.
}
\examples{
data <- list(c(1, 2), c(4), c(2, 3),c(6,8),c(12,20),c(20,30))
k <- nkur(data)
print(k)
}
