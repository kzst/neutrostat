#-----------------------------------------------------------------------------#
#                                                                             #
#         R Package for Neutrosophic Statitistics                             #
#                                                                             #
#  Written by: Zahid Khan, Zsolt T. Kosztyan                                  #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: August 2024                                                  #
#-----------------------------------------------------------------------------#

#' @export
# Computation of quantile values of the neutrosophic numbers
nquant <- function(data) {
  # Sorting neutrosophic values
  SI <- interval_sort(data)

  # computing Q2
  n <- length(SI)
  if (n %% 2 == 1) {
    # Odd number of intervals
    Q2 <- SI[[ceiling(n / 2)]]
    lower_half <- SI[1:(ceiling(n / 2) - 1)]
    upper_half <- SI[(ceiling(n / 2) + 1):n]
  } else {
    # Even number of intervals
    middle1 <- SI[[n / 2]]
    middle2 <- SI[[n / 2 + 1]]
    sum_intervals <- interval_add(list(middle1, middle2))
    Q2 <- interval_div(list(sum_intervals, c(2, 2)))
    lower_half <- SI[1:(n / 2 - 1)]
    upper_half <- SI[(n / 2 + 1):n]
  }

  # computing Q1
  n_lower <- length(lower_half)
  if (n_lower > 0) {
    if (n_lower %% 2 == 1) {
      Q1 <- lower_half[[ceiling(n_lower / 2)]]
    } else {
      middle1 <- lower_half[[n_lower / 2]]
      middle2 <- lower_half[[n_lower / 2 + 1]]
      sum_intervals <- interval_add(list(middle1, middle2))
      Q1 <- interval_div(list(sum_intervals, c(2, 2)))
    }
  } else {
    Q1 <- NULL  # No lower half to calculate Q1
  }

  # Computing Q3
  n_upper <- length(upper_half)
  if (n_upper > 0) {
    if (n_upper %% 2 == 1) {
      Q3 <- upper_half[[ceiling(n_upper / 2)]]
    } else {
      middle1 <- upper_half[[n_upper / 2]]
      middle2 <- upper_half[[n_upper / 2 + 1]]
      sum_intervals <- interval_add(list(middle1, middle2))
      Q3 <- interval_div(list(sum_intervals, c(2, 2)))
    }
  } else {
    Q3 <- NULL  # No upper half to calculate Q3
  }

  # Return a named list of quantiles
  return(list(Qu_1 = Q1, Qu_2 = Q2, Qu_3 = Q3))
}



