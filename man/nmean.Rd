\name{nmean}
\alias{nmean}
\title{ Mean of the neutrosophic data}
\description{Neutrosophic mean is an interval value of the neutrosphic numbers}
\usage{
nmean(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
Interval mean value.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
}
\author{
Zahid Khan
}
\seealso{
\code{\link{interval_add}},\code{\link{interval_div}}.
}
\examples{
data <- list(c(1, 2), c(4), c(2, 3))
mean <- nmean(data)
print(mean)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
