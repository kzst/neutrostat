\name{interval_sort}
\alias{interval_sort}
\title{ Sorting of the neutrosophic data}
\description{Sorting of neutrosophic values in the ascending order}
\usage{
interval_sort(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
List of intervals in asceding order.
}
\references{
Moore, R. E. (1979): Methods and applications of interval analysis.SIAM. doi:10.1137/1.9781611970906
}
\author{
Zahid Khan
}
\seealso{
\code{\link{interval_add}},\code{\link{interval_div}}.
}
\examples{
data <- list(c(5, 10), c(4,6), c(2, 3))
sort <- interval_sort(data)
print(sort)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
