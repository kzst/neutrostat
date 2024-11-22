\name{nexp}
\alias{nexp}
\alias{dnexp}
\alias{pnexp}
\alias{qnexp}
\alias{rnexp}
\alias{plot_npdfexp}
\alias{plot_ncdfexp}

\title{
Neutrosophic Exponential Distribution with Characteristics
}
\description{
Computes various properties of the Neutrosophic Exponential distribution, including its density, cumulative distribution function (CDF), quantiles,random numbers with summary statistics,PDF and CDF plots of the distribution.
}
\usage{
dnexp(x, rate_l, rate_u)

pnexp(q, rate_l, rate_u)

qnexp(p, rate_l, rate_u)

rnexp(n, rate_l, rate_u, stats=FALSE)

plot_npdfexp(rate_l, rate_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Exponential Distribution",
                         x.label = "x", y.label = "Density")

plot_ncdfexp(rate_l, rate_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Exponential Distribution",
                         x.label = "x", y.label = "Cumulative Probability")
}

\arguments{
  \item{x}{
    A numeric vector of observations for which the function will compute the corresponding distribution values.
  }
  \item{n}{number of random generated values}
  \item{rate_l}{
    A positive numeric value representing the lower bound of the rate parameter of the Neutrosophic Exponential distribution.
  }
  \item{rate_u}{
    A positive numeric value representing the upper bound of the rate parameter of the Neutrosophic Exponential distribution. This must be greater than or equal to \code{rate_l}.
  }
\item{p}{A vector of probabilities for which the function will compute the corresponding quantile values}
\item{q}{A vector of quantiles for which the function will compute the corresponding CDF values}
\item{stats}{
    Logical; if \code{TRUE}, the function returns summary statistics of the generated random data (e.g., mean, standard deviation, quantiles, skewness, and kurtosis).
  }
  \item{color.fill}{
    A string representing the color for neutrosophic region.
  }
  \item{color.line}{
    A string representing the color used for the line of the PDF or CDF in the plots.
  }
  \item{title}{
    A string representing the title of the plot.
  }
  \item{x.label}{
    A string representing the label for the x-axis.
  }
  \item{y.label}{
    A string representing the label for the y-axis.
  }
}
\details{
The function computes various properties of the Neutrosophic Exponential distribution. Depending on the function variant used (e.g., density, CDF, quantiles), it will return the corresponding statistical measure for each input value of \code{x} in case of random number generation from Neutrosophic Exponential distribution. Moreover basic plots of PDF and CDF can be visualized.
}
\value{
\code{dnexp} returns the PDF values

 \code{pnexp} returns the lower tail CDF values.

 \code{qnexp} returns the quantile values

 \code{rnexp} return random values with summary statistics of the simulated data

 \code{plot_npdfexp} returns PDF plot at given values of rate parameter

 \code{plot_ncdfexp} returns CDF plot at given values of rate parameter
}
\references{
Duan, W., Q., Khan, Z., Gulistan, M., Khurshid, A. (2021). Neutrosophic
Exponential Distribution: Modeling and Applications for Complex Data Analysis,
Complexity, 2021, 1-8.doi:10.1155/2021/5970613
}
\author{
Zahid Khan
}

\examples{

# random number with summary statistics
rnexp(10, rate_l=2, rate_u=4, stats = TRUE)

# PDF values
x <- c(1, 2, 3)  # Values at which to evaluate the PDF
rate_l <- 0.5
rate_u <- 2.0
dnexp(x, rate_l, rate_u)

# CDF values
q <- c(2, 3, 3.5)
rate_l <- 0.5
rate_u <- 2.0
pnexp(q, rate_l, rate_u)

# Quantile values

p <- 0.5    # Probability at which to evaluate the quantile
rate_l <- 0.5
rate_u <- 2.0
qnexp(p, rate_l, rate_u)

# PDF PLOT

plot_npdfexp(rate_l = 1, rate_u = 2, x = c(0, 5))

# CDF PLOT

 plot_ncdfexp(rate_l = 1, rate_u = 2, x = c(0, 5))

}
