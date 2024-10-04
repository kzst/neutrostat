\name{interval_div}
\alias{interval_div}

\title{Division of the neutrosophic numbers}
\description{
This function is used to find an interval division of the neutrosophic numbers
}
\usage{
interval_div(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{List of neutrosophic numbers.This numeric list contains at least two neutrosophic intervals. Each interval value should contains two elements, lower and upper.If it crisp value is used,it is considered as an interval with same upper and lower value.  }
}

\value{A numeric vector of length 2,indicating a divided value of neutrosophic intervals}
\references{
Moore, R. E. (1979): Methods and applications of interval analysis.SIAM. doi:10.1137/1.9781611970906.
Smarandache, F (2022):Neutrosophic Statistics is an extension of Interval Statistics, while Plithogenic Statistics is the most general form of statistics(second version).Internation journal of neutrosophic science. 19(1),pp.148-165. doi:10.54216/IJNS.190111

}
\author{
Zahid Khan
}
\seealso{
\code{\link{interval_mul}}.
}

\examples{
#Division of neutrosopic numbers
x=list(c(8,4),c(2,4))
interval_div(x)
}
%
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

