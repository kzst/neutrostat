#-----------------------------------------------------------------------------#
#                                                                             #
#         R Package for Neutrosophic Statitistics                             #
#                                                                             #
#  Written by: Zahid Khan, Zsolt T. Kosztyan                                  #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: November 2024                                                  #
#-----------------------------------------------------------------------------#
# Neutrosophic Exponential Distribution
#' @export
# PDF
dnexp <- function(x, rate_l, rate_u) {
  # Check if the interval is valid
  if (rate_l <= 0 || rate_u <= 0 || rate_l > rate_u) {
    stop("Invalid interval: rate_l and rate_u should be positive, and rate_l <= rate_u.")
  }


  rate <- c(rate_l, rate_u)


  min_pdf_values <- numeric(length(x))
  max_pdf_values <- numeric(length(x))

  for (i in seq_along(x)) {
    pdf_values <- ntsDists::dnsExp(x[i], rate = rate)
    min_pdf_values[i] <- min(pdf_values)
    max_pdf_values[i] <- max(pdf_values)
  }


  result <- data.frame(PDF_l = min_pdf_values, PDF_u = max_pdf_values)
  return(result)
}
#' @export
# CDF

pnexp <- function(q, rate_l, rate_u) {
  if (rate_l <= 0 || rate_u <= 0 || rate_l > rate_u) {
    stop("Invalid interval: rate_l and rate_u should be positive, and rate_l <= rate_u.")
  }

  rate <- c(rate_l, rate_u)
  cd1 <- numeric(length(q))
  cd2 <- numeric(length(q))

  for (i in seq_along(q)) {
    ncdf <- ntsDists::pnsExp(q[i], rate = rate,lower.tail = TRUE)
    cd1[i] <- min(ncdf)
    cd2[i] <- max(ncdf)
  }

  # Return data frame with only CDF bounds (excluding q values)
  result <- data.frame(CDF_l = cd1, CDF_u = cd2)
  return(result)
}

#' @export
# Quantile Function
qnexp <- function(p, rate_l, rate_u) {
  # Check if the interval is valid
  if (rate_l <= 0 || rate_u <= 0 || rate_l > rate_u) {
    stop("Invalid interval: rate_l and rate_u should be positive, and rate_l <= rate_u.")
  }

  if (p < 0 || p > 1) {
    stop("Probability p must be between 0 and 1.")
  }

  rate <- c(rate_l, rate_u)
  quantiles <- ntsDists::qnsExp(p, rate = rate)


  return(c(min(quantiles), max(quantiles)))
}
#' @export
# Random number generation
rnexp <- function(n, rate_l, rate_u, stats = FALSE) {

  if (rate_l <= 0 || rate_u <= 0 || rate_l > rate_u) {
    stop("Invalid interval: rate_l and rate_u should be positive, and rate_l <= rate_u.")
  }

  # Create a numeric vector for rates
  rate <- c(rate_l, rate_u)


  samples <- ntsDists::rnsExp(n, rate = rate)

  if (stats) {

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


    return(list(samples = samples, stats = stats))
  } else {

    return(list(samples = samples))
  }
}

#' @export
# PDF plot
plot_npdfexp <- function(rate_l, rate_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Exponential Distribution",
                         x.label = "x", y.label = "Density") {

  if (length(x) != 2) {
    stop("x must be a range of values, e.g., c(0, 5).")
  }


  x_vals <- seq(x[1], x[2], length.out = 100)


  pdf_bounds <- sapply(x_vals, function(x) {
    pdf_result <- dnexp(x, rate_l, rate_u)
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
plot_ncdfexp <- function(rate_l, rate_u, x = c(0, 5),
                         color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Exponential Distribution",
                         x.label = "x", y.label = "Cumulative Probability") {

  if (length(x) != 2) {
    stop("x must be a range of values, e.g., c(0, 5).")
  }


  x_vals <- seq(x[1], x[2], length.out = 100)


  cdf_bounds <- pnexp(x_vals, rate_l, rate_u)


  cdf_data <- data.frame(
    x = x_vals,
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







