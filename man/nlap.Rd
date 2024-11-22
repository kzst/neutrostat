\name{nlap}
\alias{nlap}
\alias{dnlap}
\alias{pnlap}
\alias{qnlap}
\alias{rnlap}
\alias{plot_npdflap}
\alias{plot_ncdflap}

\title{
Neutrosophic Laplace Distribution with Characteristics
}
\description{
Computes various properties of the Neutrosophic Laplace distribution, including its density, cumulative distribution function (CDF), quantiles,random numbers with summary statistics,PDF and CDF plots of the distribution.
}
\usage{
dnlap(x, scale_l, scale_u, location_l, location_u)

pnlap(q, scale_l, scale_u, location_l, location_u)

qnlap(p, scale_l, scale_u, location_l, location_u)

rnlap(n, scale_l, scale_u, location_l, location_u, stats = FALSE)

plot_npdflap(scale_l, scale_u, location_l, location_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Laplace Distribution",
                         x.label = "x", y.label = "Density")

plot_ncdflap(scale_l, scale_u, location_l, location_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Laplace Distribution",
                         x.label = "x", y.label = "Cumulative Probability")
}

\arguments{
  \item{x}{
    A numeric vector of observations for which the function will compute the corresponding distribution values.
  }
  \item{n}{number of random generated values}
  \item{scale_l}{
    A positive numeric value representing the lower bound of the scale parameter of the Neutrosophic Laplace distribution.
  }
  \item{scale_u}{
    A positive numeric value representing the upper bound of the scale parameter of the Neutrosophic Laplace distribution. This must be greater than or equal to \code{rate_l}.
  }
  \item{location_l}{
    A positive numeric value representing the lower bound of the location parameter of the Neutrosophic Laplace distribution.
  }
  \item{location_u}{
    A positive numeric value representing the upper bound of the location parameter of the Neutrosophic Laplace distribution.
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
The function computes various properties of the Neutrosophic Laplace distribution. Depending on the function variant used (e.g., density, CDF, quantiles), it will return the corresponding statistical measure for each input value of \code{x} in case of random number generation from Neutrosophic Laplace distribution. Moreover basic plots of PDF and CDF can be visualized.
}
\value{
\code{dnlap} returns the PDF values

 \code{pnlap} returns the lower tail CDF values.

 \code{qnlap} returns the quantile values

 \code{rnlap} return random values with summary statistics of the simulated data

 \code{plot_npdflap} returns PDF plot at given values of distributional parameters

 \code{plot_ncdflap} returns CDF plot at given values of distributional parameters
}
\references{
Musa A, Khan Z. (2024). Neutrosophic Laplace Distribution with Properties and Applications in Decision Making. International Journal of Neutrosophic Science, 2024, 73-84. doi:10.54216/IJNS.230106.
}
\author{
Zahid Khan
}

\examples{

# random number Generation with summary statistics

rnlap(10, scale_l = 2, scale_u = 4, location_l = 1, location_u = 1, stats = TRUE)

# PDF values
x <- 2
scale_l <- 0.5
scale_u <- 1
location_l<-0
location_u<-0
dnlap(x, scale_l, scale_u, location_l, location_u)

# CDF values
q <- 1.5
scale_l <- 1
scale_u <- 2
location_l<-0
location_u<-0
pnlap(q, scale_l, scale_u, location_l, location_u)

# Quantile values

p <- 0.1
scale_l <- 0.5
scale_u <- 0.7
location_l<-0
location_u<-0
qnlap(p, scale_l, scale_u, location_l, location_u)

# PDF PLOT
scale_l <- 0.5
scale_u <- 1
location_l<-0
location_u<-0
plot_npdflap(scale_l, scale_u, location_l, location_u, x = c(-5, 5))

# CDF PLOT
scale_l <- 0.5
scale_u <- 1
location_l<-0
location_u<-0
plot_ncdflap(scale_l, scale_u, location_l, location_u, x = c(-5, 5))

}
