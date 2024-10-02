\name{nvar}
\alias{nvar}
\title{Variance of the neutrosophic data}
\description{Neutrosophic variance is an interval value of the neutrosphic numbers}
\usage{
nvar(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
Interval dispersion value.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
}
\author{
Zahid Khan
}
\seealso{
\code{\link{nmean}},\code{\link{interval_add}}.
}
\examples{
data <- list(6, c(2, 5), 30, c(18, 24))
variance <- nvar(data)
print(variance)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
