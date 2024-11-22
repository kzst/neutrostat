\name{nnorm}
\alias{nnorm}
\alias{dnnorm}
\alias{pnnorm}
\alias{qnnorm}
\alias{rnnorm}
\alias{plot_npdfnorm}
\alias{plot_ncdfnorm}

\title{
Neutrosophic Normal Distribution with Characteristics
}
\description{
Computes various properties of the Neutrosophic Normal distribution, including its density, cumulative distribution function (CDF), quantiles,random numbers with summary statistics,PDF and CDF plots of the distribution.
}
\usage{
dnnorm(x, sd_l, sd_u, mean_l, mean_u)

pnnorm(q, sd_l, sd_u, mean_l, mean_u)

qnnorm(p, sd_l, sd_u, mean_l, mean_u)

rnnorm(n, sd_l, sd_u, mean_l, mean_u, stats = FALSE)

plot_npdfnorm(sd_l, sd_u, mean_l, mean_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Normal Distribution",
                         x.label = "x", y.label = "Density")

plot_ncdfnorm(sd_l, sd_u, mean_l, mean_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Normal Distribution",
                         x.label = "x", y.label = "Cumulative Probability")
}

\arguments{
  \item{x}{
    A numeric vector of observations for which the function will compute the corresponding distribution values.
  }
  \item{n}{number of random generated values}
  \item{sd_l}{
    A positive numeric value representing the lower bound of the sd parameter of the Neutrosophic Normal distribution.
  }
  \item{sd_u}{
    A positive numeric value representing the upper bound of the sd parameter of the Neutrosophic Normal distribution. This must be greater than or equal to \code{rate_l}.
  }
  \item{mean_l}{
    A  numeric value representing the lower bound of the mean parameter of the Neutrosophic Normal distribution.
  }
  \item{mean_u}{
    A  numeric value representing the upper bound of the mean parameter of the Neutrosophic Normal distribution.
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
The function computes various properties of the Neutrosophic Normal distribution. Depending on the function variant used (e.g., density, CDF, quantiles), it will return the corresponding statistical measure for each input value of \code{x} in case of random number generation from Neutrosophic Normal distribution. Moreover basic plots of PDF and CDF can be visualized.
}
\value{
\code{dnnorm} returns the PDF values

 \code{pnnorm} returns the lower tail CDF values.

 \code{qnnorm} returns the quantile values

 \code{rnnorm} return random values with summary statistics of the simulated data

 \code{plot_npdfnorm} returns PDF plot at given values of distributional parameters

 \code{plot_ncdfnorm} returns CDF plot at given values of distributional parameters
}
\references{

Patro SK, Smarandache F. (2016). The neutrosophic statistical distribution, more problems, more solutions. Neutrosophic Sets and Systems, 12, 73-79.doi:10.5281/zenodo.571153
}
\author{
Zahid Khan
}

\examples{

# random number Generation with summary statistics

rnnorm(10, sd_l = 2, sd_u = 4, mean_l = 1, mean_u = 1, stats = TRUE)

# PDF values
x <- 2
sd_l <- 0.5
sd_u <- 1
mean_l<-0
mean_u<-0
dnnorm(x, sd_l, sd_u, mean_l, mean_u)

# CDF values
q <- 1.5
sd_l <- 1
sd_u <- 2
mean_l<-0
mean_u<-0
pnnorm(q, sd_l, sd_u, mean_l, mean_u)

# Quantile values

p <- 0.1
sd_l <- 0.5
sd_u <- 0.7
mean_l<-0
mean_u<-0
qnnorm(p, sd_l, sd_u, mean_l, mean_u)

# PDF PLOT
sd_l <- 0.5
sd_u <- 1
mean_l<-0
mean_u<-0
plot_npdfnorm(sd_l, sd_u, mean_l, mean_u, x = c(-5, 5))

# CDF PLOT
sd_l <- 0.5
sd_u <- 1
mean_l<-0
mean_u<-0
plot_ncdfnorm(sd_l, sd_u, mean_l, mean_u, x = c(-5, 5))

}
