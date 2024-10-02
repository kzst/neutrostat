\name{nmedian}
\alias{nmedian}
\title{ Median of the neutrosophic data}
\description{Finding the median of the neutrosophic interval values}
\usage{
nmedian(data)
}

\arguments{
  \item{data}{list of  neutrosophic numbers}
}

\value{
interval median value.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
}
\author{
Zahid Khan
}
\seealso{
\code{\link{interval_sort}}.
}
\examples{
data <- list(c(5, 10), c(4,6), c(2, 3))
med <- nmedian(data)
print(med)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
