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
# function is used to find median of neutrosophic numbers
nmedian <- function(data) {
  # Sorting first
  sorted_intervals <- interval_sort(data)

  # median based on sorted values
  n <- length(sorted_intervals)
  if (n %% 2 == 1) {
    # If odd number of intervals, take the middle one
    median_interval <- sorted_intervals[[ceiling(n / 2)]]
  } else {
    # If even number of intervals, average the two middle intervals
    middle1 <- sorted_intervals[[n / 2]]
    middle2 <- sorted_intervals[[n / 2 + 1]]
    sum_intervals <- interval_add(list(middle1, middle2))
    median_interval <- interval_div(list(sum_intervals, c(2, 2)))
  }

  return(list(Neutrosophic_median = median_interval))
}


