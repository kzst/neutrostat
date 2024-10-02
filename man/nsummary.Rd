\name{nsummary}
\alias{nsummary}
\title{ summary of the neutrosophic data}
\description{ Descriptive summary of the neutrosphic numbers}
\usage{
nsummary(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
Data frame of descriptive neutrosophic statistics.
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
data <- list(c(1, 2), c(4), c(2, 3),c(5,11),c(4,8),c(20,25))
s <- nsummary(data)
print(s)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
