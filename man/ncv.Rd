\name{ncv}
\alias{ncv}
\title{ CV of the neutrosophic data}
\description{Neutrosophic coefficient of variation is an interval value of the neutrosphic numbers}
\usage{
ncv(data)
}

\arguments{
  \item{data}{data is a list of  neutrosophic numbers}
}

\value{
Interval cv value.
}
\references{
Florentin Smarandache (2014): Introduction to Neutrosophic Statistics. ISBN: 9781599732749
Hussein Al-Marshadi, Ali and Aslam, Muhammad and Abdullah, Alharbey (2021): Uncertainty-Based Trimmed Coefficient of Variation with Application, Journal of Mathematics, 2021(1), pages 5511904. Wiley Online Library. doi:10.1155/2021/5511904

Kandemir, Hacer Şengül and Aral, Nazlım Deniz and Karakaş, Murat and Et, Mikail (2024): Neutrosophic Statistical Analysis of Temperatures of Cities in the Southeastern Anatolia Region of Turkey, Neutrosophic Systems with Applications, 14, pp. 50-59. doi:10.61356/j.nswa.2024.119
}
\author{
Zahid Khan
}
\seealso{
\code{\link{nmean}},\code{\link{nstd}}.
}
\examples{
data <- list(c(1, 2), c(4), c(2, 3))
mean <- nmean(data)
print(mean)
}
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
