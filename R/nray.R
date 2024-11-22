#-----------------------------------------------------------------------------#
#                                                                             #
#         R Package for Neutrosophic Statistics                             #
#                                                                             #
#  Written by: Zahid Khan, Zsolt T. Kosztyan                                  #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: November 2024                                                  #
#-----------------------------------------------------------------------------#
# Neutrosophic Rayleigh Distribution
#' @import ggplot2
#' @import ntsDists
#' @import moments
#' @import stats

#' @export
# PDF
dnray <- function(x, scale_l, scale_u) {
  # Check if the interval is valid
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }
  if (any(x < 0)) {
    stop("Invalid domain: all values of x must be >= 0.")
  }
  # Use dnsExp from ntsDists package to calculate the interval PDF for each value in x
  theta <- c(scale_l, scale_u)

  # Initialize lists to store min and max PDF values for each x
  min_pdf_values <- numeric(length(x))
  max_pdf_values <- numeric(length(x))


  for (i in seq_along(x)) {
    pdf_values <- ntsDists::dnsRayleigh(x[i], theta)
    min_pdf_values[i] <- min(pdf_values)
    max_pdf_values[i] <- max(pdf_values)
  }


  result <- data.frame(PDF_l = min_pdf_values, PDF_u = max_pdf_values)
  return(result)
}


#' @export
# CDF
pnray <- function(q, scale_l, scale_u) {
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }

  theta <- c(scale_l, scale_u)
  cd1 <- numeric(length(q))
  cd2 <- numeric(length(q))

  for (i in seq_along(q)) {
    ncdf <- ntsDists::pnsRayleigh(q[i], theta=theta, lower.tail = TRUE)
    cd1[i] <- min(ncdf)
    cd2[i] <- max(ncdf)
  }


  result <- data.frame(CDF_l = cd1, CDF_u = cd2)
  return(result)
}


#' @export
# Quantile function
qnray <- function(p, scale_l, scale_u) {
  # Check if the interval is valid
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }


  if (p < 0 || p > 1) {
    stop("Probability p must be between 0 and 1.")
  }

  theta <- c(scale_l, scale_u)
  quantiles <- ntsDists::qnsRayleigh(p, theta)


  return(c(min(quantiles), max(quantiles)))
}


#' @export
# random number generation
rnray <- function(n, scale_l, scale_u, stats = FALSE) {

  # Check if the interval is valid
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }


  theta <- c(scale_l, scale_u)

  # Use rnsExp from ntsDists package for random sample generation
  samples <- ntsDists::rnsRayleigh(n, theta)

  if (stats) {
    # Calculate the neutrosophic statistics for each column of samples
    stats <- data.frame(
      Statistic = c("Mean", "SD", "Q1", "Median", "Q3", "Skewness", "Kurtosis"),
      Simulated_Value = I(lapply(list(
        c(mean(samples[, 1]), mean(samples[, 2])),
        c(stats::sd(samples[, 1]), stats::sd(samples[, 2])),
        c(stats::quantile(samples[, 1], 0.25), stats::quantile(samples[, 2], 0.25)),
        c(stats::median(samples[, 1]), stats::median(samples[, 2])),
        c(stats::quantile(samples[, 1], 0.75), stats::quantile(samples[, 2], 0.75)),
        c(moments::skewness(samples[, 1]), moments::skewness(samples[, 2])),
        c(moments::kurtosis(samples[, 1]), moments::kurtosis(samples[, 2]))
      ), round, 3))  # Round each value to 3 decimal places
    )

    # Return both the samples and the neutro summary as a list
    return(list(samples = samples, stats = stats))
  } else {
    # Return random samples only
    return(list(samples = samples))
  }
}
utils::globalVariables(c("l", "u"))
#' @export
# PDF plot
plot_npdfray <- function(scale_l, scale_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Rayleigh Distribution",
                         x.label = "x", y.label = "Density") {
  # Check if x is a valid range
  if (length(x) != 2) {
    stop("x must be a range of values, e.g., c(0, 5).")
  }


  x_vals <- seq(x[1], x[2], length.out = 100)


  pdf_bounds <- sapply(x_vals, function(x) {
    pdf_result <- dnray(x, scale_l, scale_u)
    c(l = pdf_result$PDF_l, u = pdf_result$PDF_u)
  })


  pdf_data <- data.frame(
    x = x_vals,
    l = pdf_bounds["l", ],
    u = pdf_bounds["u", ]
  )

  # Plot the PDF with shaded area between bounds
  ggplot(pdf_data, aes(x = x)) +
    geom_ribbon(aes(ymin = l, ymax = u), fill = color.fill, alpha = 0.5) +
    geom_line(aes(y = l), color = color.line, linetype = "dashed") +
    geom_line(aes(y = u), color = color.line, linetype = "dashed") +
    labs(title = title,
         x = x.label,
         y = y.label) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),              # Remove grid lines
      axis.line = element_line(color = "black")  # Add x and y axis borders
    )
}

#' @export
# CDF plot
plot_ncdfray <- function(scale_l, scale_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Rayleigh Distribution",
                         x.label = "x", y.label = "Cumulative Probability") {
  # Check if x_range is a single value
  if (length(x) != 2) {
    stop("x_range must be a range of values, e.g., c(0, 5).")
  }


  x <- seq(x[1], x[2], length.out = 100)


  cdf_bounds <- pnray(x, scale_l, scale_u)


  cdf_data <- data.frame(
    x = x,
    l = cdf_bounds$CDF_l,
    u = cdf_bounds$CDF_u
  )

  # Plot the neutrosophic CDF
  ggplot(cdf_data, aes(x = x)) +
    geom_ribbon(aes(ymin = l, ymax = u), fill = color.fill, alpha = 0.5) +
    geom_line(aes(y = l), color = color.line, linetype = "dashed") +
    geom_line(aes(y = u), color = color.line, linetype = "dashed") +
    labs(title = title,
         x = x.label,
         y = y.label) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),              # Remove grid lines
      axis.line = element_line(color = "black")  # Add x and y axis borders
    )
}








