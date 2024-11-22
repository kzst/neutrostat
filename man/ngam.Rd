\name{ngam}
\alias{ngam}
\alias{dngam}
\alias{pngam}
\alias{qngam}
\alias{rngam}
\alias{plot_npdfgam}
\alias{plot_ncdfgam}

\title{
Neutrosophic Gamma Distribution with Characteristics
}
\description{
Computes various properties of the Neutrosophic Gamma distribution, including its density, cumulative distribution function (CDF), quantiles,random numbers with summary statistics,PDF and CDF plots of the distribution.
}
\usage{
dngam(x, scale_l, scale_u, shape_l, shape_u)

pngam(q, scale_l, scale_u, shape_l, shape_u)

qngam(p, scale_l, scale_u, shape_l, shape_u)

rngam(n, scale_l, scale_u, shape_l, shape_u, stats = FALSE)

plot_npdfgam(scale_l, scale_u, shape_l, shape_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Gamma Distribution",
                         x.label = "x", y.label = "Density")

plot_ncdfgam(scale_l, scale_u, shape_l, shape_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Gamma Distribution",
                         x.label = "x", y.label = "Cumulative Probability")
}

\arguments{
  \item{x}{
    A numeric vector of observations for which the function will compute the corresponding distribution values.
  }
  \item{n}{number of random generated values}
  \item{scale_l}{
    A positive numeric value representing the lower bound of the scale parameter of the Neutrosophic Gamma distribution.
  }
  \item{scale_u}{
    A positive numeric value representing the upper bound of the scale parameter of the Neutrosophic Gamma distribution. This must be greater than or equal to \code{rate_l}.
  }
  \item{shape_l}{
    A positive numeric value representing the lower bound of the shape parameter of the Neutrosophic Gamma distribution.
  }
  \item{shape_u}{
    A positive numeric value representing the upper bound of the shape parameter of the Neutrosophic Gamma distribution.
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
The function computes various properties of the Neutrosophic Gamma distribution. Depending on the function variant used (e.g., density, CDF, quantiles), it will return the corresponding statistical measure for each input value of \code{x} in case of random number generation from Neutrosophic Gamma distribution. Moreover basic plots of PDF and CDF can be visualized.
}
\value{
\code{dngam} returns the PDF values

 \code{pngam} returns the lower tail CDF values.

 \code{qngam} returns the quantile values

 \code{rngam} return random values with summary statistics of the simulated data

 \code{plot_npdfgam} returns PDF plot at given values of distributional parameters

 \code{plot_ncdfgam} returns CDF plot at given values of distributional parameters
}
\references{
Khan Z, Al-Bossly A, Almazah M, Alduais FS. (2021). On Statistical Development of Neutrosophic Gamma Distribution with Applications to Complex Data Analysis,
Complexity, 2021, 1-8.doi:10.1155/2021/3701236
}
\author{
Zahid Khan
}

\examples{

# random number Generation with summary statistics

rngam(10, scale_l = 2, scale_u = 4, shape_l = 1, shape_u = 1, stats = TRUE)

# PDF values
x <- 2
scale_l <- 1
scale_u <- 2.0
shape_l<-0.5
shape_u<-2
dngam(x, scale_l, scale_u, shape_l, shape_u)

# CDF values
q <- 1.5
scale_l <- 1
scale_u <- 2.0
shape_l<-0.5
shape_u<-2.0
pngam(q, scale_l, scale_u, shape_l, shape_u)

# Quantile values

p <- 0.5
scale_l <- 1
scale_u <- 2.0
shape_l<-0.5
shape_u<-2
qngam(p, scale_l, scale_u, shape_l, shape_u)

# PDF PLOT
scale_l <- 1
scale_u <- 1
shape_l<-2
shape_u<-3
plot_npdfgam(scale_l, scale_u, shape_l, shape_u, x = c(0, 5))

# CDF PLOT
scale_l <- 1
scale_u <- 1
shape_l<-2
shape_u<-3
plot_ncdfgam(scale_l, scale_u, shape_l, shape_u, x = c(0, 5))

}
