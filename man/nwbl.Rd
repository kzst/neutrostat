\name{nwbl}
\alias{nwbl}
\alias{dnwbl}
\alias{pnwbl}
\alias{qnwbl}
\alias{rnwbl}
\alias{plot_npdfwbl}
\alias{plot_ncdfwbl}

\title{
Neutrosophic Weibull Distribution with Characteristics
}
\description{
Computes various properties of the Neutrosophic Weibull distribution, including its density, cumulative distribution function (CDF), quantiles,random numbers with summary statistics,PDF and CDF plots of the distribution.
}
\usage{
dnwbl(x, scale_l, scale_u, shape_l, shape_u)

pnwbl(q, scale_l, scale_u, shape_l, shape_u)

qnwbl(p, scale_l, scale_u, shape_l, shape_u)

rnwbl(n, scale_l, scale_u, shape_l, shape_u, stats = FALSE)

plot_npdfwbl(scale_l, scale_u, shape_l, shape_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Weibull Distribution",
                         x.label = "x", y.label = "Density")

plot_ncdfwbl(scale_l, scale_u, shape_l, shape_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Weibull Distribution",
                         x.label = "x", y.label = "Cumulative Probability")
}

\arguments{
  \item{x}{
    A numeric vector of observations for which the function will compute the corresponding distribution values.
  }
  \item{n}{number of random generated values}
  \item{scale_l}{
    A positive numeric value representing the lower bound of the scale parameter of the Neutrosophic Weibull distribution.
  }
  \item{scale_u}{
    A positive numeric value representing the upper bound of the scale parameter of the Neutrosophic Weibull distribution. This must be greater than or equal to \code{rate_l}.
  }
  \item{shape_l}{
    A positive numeric value representing the lower bound of the shape parameter of the Neutrosophic Weibull distribution.
  }
  \item{shape_u}{
    A positive numeric value representing the upper bound of the shape parameter of the Neutrosophic Weibull distribution.
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
The function computes various properties of the Neutrosophic Weibull distribution. Depending on the function variant used (e.g., density, CDF, quantiles), it will return the corresponding statistical measure for each input value of \code{x} in case of random number generation from Neutrosophic Weibull distribution. Moreover basic plots of PDF and CDF can be visualized.
}
\value{
\code{dnwbl} returns the PDF values

 \code{pnwbl} returns the lower tail CDF values.

 \code{qnwbl} returns the quantile values

 \code{rnwbl} return random values with summary statistics of the simulated data

 \code{plot_npdfwbl} returns PDF plot at given values of distributional parameters

 \code{plot_ncdfwbl} returns CDF plot at given values of distributional parameters
}
\references{
Khan, Kahid; Gulistan, Muhammad; Lane-Krebs, Katrina; Salem, Sultan (2023). Neutrophasic Weibull model with applications to survival studies. CQUniversity, 25-42.doi:10.1016/B978-0-323-99456-9.00007-6
}
\author{
Zahid Khan
}

\examples{

# random number Generation with summary statistics

rnwbl(5, scale_l = 2, scale_u = 4, shape_l = 1, shape_u = 1, stats = TRUE)

# PDF values
x <- 2
scale_l <- 1
scale_u <- 2.0
shape_l<-0.5
shape_u<-2
dnwbl(x, scale_l, scale_u, shape_l, shape_u)

# CDF values
q <- 1.5
scale_l <- 1
scale_u <- 2.0
shape_l<-0.5
shape_u<-2.0
pnwbl(q, scale_l, scale_u, shape_l, shape_u)

# Quantile values

p <- 0.5
scale_l <- 1
scale_u <- 2.0
shape_l<-0.5
shape_u<-2
qnwbl(p, scale_l, scale_u, shape_l, shape_u)

# PDF PLOT
scale_l <- 1
scale_u <- 1
shape_l<-2
shape_u<-3
plot_npdfwbl(scale_l, scale_u, shape_l, shape_u, x = c(0, 5))

# CDF PLOT
scale_l <- 1
scale_u <- 1
shape_l<-2
shape_u<-3
plot_ncdfwbl(scale_l, scale_u, shape_l, shape_u, x = c(0, 5))

}
