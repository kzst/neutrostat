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
# Neutrosophic Laplace Distribution
#' @export
# PDF
dnlap <- function(x, scale_l, scale_u, location_l, location_u) {

  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }
  if (location_l > location_u) {
    stop("location_l <= location_u.")
  }

  scales <- c(scale_l, scale_u)
  locations <- c(location_l, location_u)

  min_pdf_values <- numeric(length(x))
  max_pdf_values <- numeric(length(x))

  for (i in seq_along(x)) {

    pdf_values <- sapply(1:2, function(j) {
      1 / (2 * scales[j]) * exp(-abs(x[i] - locations[j]) / scales[j])
    })

    # Get the min and max PDF values for the current x[i]
    min_pdf_values[i] <- min(pdf_values)
    max_pdf_values[i] <- max(pdf_values)
  }

  result <- data.frame(PDF_l = min_pdf_values, PDF_u = max_pdf_values)
  return(result)
}

#' @export
# CDF
pnlap <- function(q, scale_l, scale_u, location_l, location_u) {
  # Check if the interval is valid
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }
  if (location_l >location_u) {
    stop("location_l <= location_u.")
  }
  cdf_l <- numeric(length(q))
  cdf_u <- numeric(length(q))

  for (i in seq_along(q)) {

    cdf1 <- ifelse(
      q[i] < location_l,
      0.5 * exp((q[i] - location_l) / scale_l),
      1 - 0.5 * exp(-(q[i] - location_l) / scale_l)
    )

    cdf2 <- ifelse(
      q[i] < location_u,
      0.5 * exp((q[i] - location_u) / scale_u),
      1 - 0.5 * exp(-(q[i] - location_u) / scale_u)
    )

    cdf_l[i] <- min(cdf1, cdf2)
    cdf_u[i] <- max(cdf1, cdf2)
  }

  result <- data.frame(CDF_l = cdf_l, CDF_u = cdf_u)
  return(result)
}
#' @export
# Quantile function
qnlap <- function(p, scale_l, scale_u, location_l, location_u) {
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }
  if (location_l > location_u) {
    stop("location_l <= location_u.")
  }
  if (any(p < 0) || any(p > 1)) {
    stop("All probabilities in p must be between 0 and 1.")
  }

  scales <- c(scale_l, scale_u)
  locations <- c(location_l, location_u)

  min.quantile <- numeric(length(p))
  max.quantile <- numeric(length(p))

  for (i in seq_along(p)) {
    quantiles <- sapply(1:2, function(j) {
      if (p[i] <= 0.5) {
        locations[j] + scales[j] * log(2 * p[i])
      } else {
        locations[j] - scales[j] * log(2 * (1 - p[i]))
      }
    })


    min.quantile[i] <- min(quantiles)
    max.quantile[i] <- max(quantiles)
  }

  result <- data.frame(Quantile_l = min.quantile, Quantile_u = max.quantile)

  return(result)
}


#' @export
# random number generation
rnlap <- function(n, scale_l, scale_u, location_l, location_u, stats = FALSE) {
  if (scale_l <= 0 || scale_u <= 0 || scale_l > scale_u) {
    stop("Invalid interval: scale_l and scale_u should be positive, and scale_l <= scale_u.")
  }
  if (location_l > location_u) {
    stop("location_l <= location_u.")
  }



samples <- qnlap(runif(n), scale_l = scale_l, scale_u = scale_u, location_l =location_l, location_u = location_u)
names(samples) <- NULL
matrix(samples)

if (stats) {
    raw_stats <- list(
      Mean = c(mean(samples[, 1]), mean(samples[, 2])),
      SD = c(sd(samples[, 1]), sd(samples[, 2])),
      Q1 = c(quantile(samples[, 1], 0.25), quantile(samples[, 2], 0.25)),
      Median = c(median(samples[, 1]), median(samples[, 2])),
      Q3 = c(quantile(samples[, 1], 0.75), quantile(samples[, 2], 0.75)),
      Skewness = c(moments::skewness(samples[, 1]), moments::skewness(samples[, 2])),
      Kurtosis = c(moments::kurtosis(samples[, 1]), moments::kurtosis(samples[, 2]))
    )

    sorted_stats <- lapply(raw_stats, function(stat) sort(stat))

    stats <- data.frame(
      Statistic = names(sorted_stats),
      Simulated_Value = I(lapply(sorted_stats, round, 3))
    )

    return(list(samples = samples, stats = stats))
  } else {
    return(list(samples = samples))
  }
}
utils::globalVariables(c("l", "u"))
#' @export
# PDF plot
plot_npdflap <- function(scale_l, scale_u, location_l, location_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Laplace Distribution",
                         x.label = "x", y.label = "Density") {
  # Check if x is a valid range
  if (length(x) != 2) {
    stop("x must be a range of values, e.g., c(0, 5).")
  }


  x_vals <- seq(x[1], x[2], length.out = 100)


  pdf_bounds <- sapply(x_vals, function(x) {
    pdf_result <- dnlap(x, scale_l, scale_u, location_l, location_u)
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
plot_ncdflap <- function(scale_l, scale_u, location_l, location_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Laplace Distribution",
                         x.label = "x", y.label = "Cumulative Probability") {
  # Check if x_range is a single value
  if (length(x) != 2) {
    stop("x_range must be a range of values, e.g., c(0, 5).")
  }

  # Generate x values
  x <- seq(x[1], x[2], length.out = 100)


  cdf_bounds <- pnlap(x, scale_l, scale_u, location_l, location_u)


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








