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
# Neutrosophic Normal Distribution
#' @export
# PDF
dnnorm<- function(x, sd_l, sd_u, mean_l, mean_u) {
  # Check if the interval is valid
  if (sd_l <= 0 || sd_u <= 0 || sd_l > sd_u) {
    stop("Invalid interval: sd_l and sd_u should be positive, and sd_l <= sd_u.")
  }
  if (mean_l > mean_u) {
    stop("mean_l <= mean_u.")
  }


  sd <- c(sd_l, sd_u)
  mean <- c(mean_l, mean_u)

  min_pdf_values <- numeric(length(x))
  max_pdf_values <- numeric(length(x))

  for (i in seq_along(x)) {
    pdf_values <- ntsDists::dnsNorm(x[i], mean=mean, sd=sd)
    min_pdf_values[i] <- min(pdf_values)
    max_pdf_values[i] <- max(pdf_values)
  }
  result <- data.frame(PDF_l = min_pdf_values, PDF_u = max_pdf_values)
  return(result)
}

#' @export
# CDF
pnnorm <- function(q, sd_l, sd_u, mean_l, mean_u) {
  # Check if the interval is valid
  if (sd_l <= 0 || sd_u <= 0 || sd_l > sd_u) {
    stop("Invalid interval: sd_l and sd_u should be positive, and sd_l <= sd_u.")
  }
  if (mean_l > mean_u) {
    stop("mean_l <= mean_u.")
  }

  sd <- c(sd_l, sd_u)
  mean <- c(mean_l, mean_u)
  cd1 <- numeric(length(q))
  cd2 <- numeric(length(q))

  for (i in seq_along(q)) {
    ncdf <- ntsDists::pnsNorm(q[i], mean=mean, sd=sd, lower.tail = TRUE)
    cd1[i] <- min(ncdf)
    cd2[i] <- max(ncdf)
  }


  result <- data.frame(CDF_l = cd1, CDF_u = cd2)
  return(result)
}


#' @export
# Quantile function
qnnorm <- function(p, sd_l, sd_u, mean_l, mean_u) {
  # Check if the interval is valid
  if (sd_l <= 0 || sd_u <= 0 || sd_l > sd_u) {
    stop("Invalid interval: sd_l and sd_u should be positive, and sd_l <= sd_u.")
  }
  if (mean_l > mean_u) {
    stop("mean_l <= mean_u.")
  }

  if (p < 0 || p > 1) {
    stop("Probability p must be between 0 and 1.")
  }

  sd <- c(sd_l, sd_u)
  mean <- c(mean_l, mean_u)
  quantiles <- ntsDists::qnsNorm(p, mean=mean, sd=sd)


  return(c(min(quantiles), max(quantiles)))
}

#' @export
# random number generation
rnnorm <- function(n, sd_l, sd_u, mean_l, mean_u, stats = FALSE) {
  if (sd_l <= 0 || sd_u <= 0 || sd_l > sd_u) {
    stop("Invalid interval: sd_l and sd_u should be positive, and sd_l <= sd_u.")
  }
  if (mean_l > mean_u) {
    stop("mean_l <= mean_u.")
  }

  sd <- c(sd_l, sd_u)
  mean <- c(mean_l, mean_u)

  samples <- ntsDists::rnsNorm(n, mean = mean, sd = sd)

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
plot_npdfnorm <- function(sd_l, sd_u, mean_l, mean_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "PDF Neutrosophic Normal Distribution",
                         x.label = "x", y.label = "Density") {
  # Check if x is a valid range
  if (length(x) != 2) {
    stop("x must be a range of values, e.g., c(0, 5).")
  }

  # Generate x values
  x_vals <- seq(x[1], x[2], length.out = 100)

  # Compute the PDF bounds using dnexp
  pdf_bounds <- sapply(x_vals, function(x) {
    pdf_result <- dnnorm(x, sd_l, sd_u, mean_l, mean_u)
    c(l = pdf_result$PDF_l, u = pdf_result$PDF_u)
  })


  pdf_data <- data.frame(
    x = x_vals,
    l = pdf_bounds["l", ],
    u = pdf_bounds["u", ]
  )


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
plot_ncdfnorm <- function(sd_l, sd_u, mean_l, mean_u, x = c(0, 5),color.fill = "lightblue", color.line = "blue",
                         title = "CDF Neutrosophic Normal Distribution",
                         x.label = "x", y.label = "Cumulative Probability") {
  # Check if x_range is a single value
  if (length(x) != 2) {
    stop("x_range must be a range of values, e.g., c(0, 5).")
  }


  x <- seq(x[1], x[2], length.out = 100)

  # Calculate CDF bounds using the pre-defined pnexp function
  cdf_bounds <- pnnorm(x, sd_l, sd_u, mean_l, mean_u)


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








