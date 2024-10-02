\name{nquant}
\alias{nquant}
\title{ Quantiles of the neutrosophic data}
\description{Neutrosophic quantiles provide three quantile interval values of the neutrosophic data}
\usage{
nquant(data)
}

\arguments{
  \item{data}{A list of  neutrosophic numbers.Each neutrosophic number is represented by an interval.}
}

\value{
A named list containing the first, second and third quantile interval values where each quantile is represented as an interval value
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
}
\author{
Zahid Khan
}
\seealso{
\code{\link{nmedian}}.
}
\examples{
data <- list(c(5, 10), c(4,6), c(2, 3),c(4,8))
q <- nquant(data)
print(q)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
