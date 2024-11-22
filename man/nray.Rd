\name{nray}
\alias{nray}
\alias{dnray}
\alias{pnray}
\alias{qnray}
\alias{rnray}
\alias{plot_npdfray}
\alias{plot_ncdfray}

\title{
Neutrosophic Rayleigh Distribution with Characteristics
}
\description{
Computes various properties of the Neutrosophic Rayleigh distribution, including its density, cumulative distribution function (CDF), quantiles,random numbers with summary statistics,PDF and CDF plots of the distribution.
}
\usage{
dnray(x, scale_l, scale_u)

pnray(q, scale_l, scale_u)

qnray(p, scale_l, scale_u)

rnray(n, scale_l, scale_u, stats=FALSE)

plot_npdfray(scale_l, scale_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Rayleigh Distribution",
                         x.label = "x", y.label = "Density")

plot_ncdfray(scale_l, scale_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Rayleigh Distribution",
                         x.label = "x", y.label = "Cumulative Probability")
}

\arguments{
  \item{x}{
    A numeric vector of observations for which the function will compute the corresponding distribution values.
  }
  \item{n}{number of random generated values}
  \item{scale_l}{
    A positive numeric value representing the lower bound of the scale parameter of the Neutrosophic Rayleigh distribution.
  }
  \item{scale_u}{
    A positive numeric value representing the upper bound of the scale parameter of the Neutrosophic Rayleigh distribution. This must be greater than or equal to \code{scale_l}.
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
The function computes various properties of the Neutrosophic Rayleigh distribution. Depending on the function variant used (e.g., density, CDF, quantiles), it will return the corresponding statistical measure for each input value of \code{x} in case of random number generation from Neutrosophic Rayleigh distribution. Moreover basic plots of PDF and CDF can be visualized.
}
\value{
\code{dnray} returns the PDF values

 \code{pnray} returns the lower tail CDF values

 \code{qnray} returns the quantile values

 \code{rnray} return random values with summary statistics of the simulated data

 \code{plot_npdfexp} returns PDF plot at given values of scale parameter

 \code{plot_ncdfexp} returns CDF plot at given values of scale parameter
}
\references{
Khan, Z., Gulistan, M., Kausar, N., Park, C. (2021). Neutrosophic Rayleigh Model With Some Basic Characteristics and Engineering Applications. IEEE Access, 9, 71277-71283. doi:10.1109/ACCESS.2021.3078150.
}
\author{
Zahid Khan
}

\examples{

# random number with summary statistics
rnray(10, scale_l=2, scale_u=4, stats = TRUE)

# PDF values
x <- c(1, 2, 3)  # Values at which to evaluate the PDF
scale_l <- 0.5
scale_u <- 2.0
dnray(x, scale_l, scale_u)

# CDF values
q <- c(2, 3, 3.5)
scale_l <- 0.5
scale_u <- 2.0
pnray(q, scale_l, scale_u)

# Quantile values

p <- 0.5    # Probability at which to evaluate the quantile
scale_l <- 0.5
scale_u <- 2.0
qnray(p, scale_l, scale_u)

# PDF PLOT

scale_l <- 0.5  # Minimum rate
scale_u <- 2  # Maximum rate
plot_npdfray(scale_l, scale_u, x = c(0, 3))

# CDF PLOT

scale_l <- 0.5  # Minimum rate
scale_u <- 2.0  # Maximum rate
plot_ncdfray(scale_l, scale_u, x = c(0, 3),title = "")


}
