\name{interval_df}
\alias{interval_df}

\title{Interval conversion for neutrosophic numbers}
\description{
Interval conversion for neutrosophic numbers
}
\usage{
interval_df(data)
}

\arguments{
  \item{data}{data is a vector or a list of  neutrosophic numbers}
}

\value{
Data frame of neutrosophic numbers.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
}
\author{
Zahid Khan
}


\examples{
# values are interval forms as required in neutrosophic data

data <- list(c(6, 6), c(2, 8), c(30,50), c(18, 24))

interval_df(data)
}

\concept{ statistics }
