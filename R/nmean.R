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
# A function to calculate the neutrosophic mean of neutrosophic data values
nmean <- function(data) {
  # Convert data to intervals dataframe
  intervals_df <- interval_df(data)

  # Convert dataframe to list of intervals
  intervals_list <- lapply(1:nrow(intervals_df), function(i) c(intervals_df$First_Value[i], intervals_df$Second_Value[i]))

  # If there's only one interval, return it as the mean
  if (length(intervals_list) == 1) {
    return(intervals_list[[1]])
  }

  # Calculate sum of intervals using the existing interval_add function
  sum_of_intervals <- interval_add(intervals_list)

  # Number of intervals
  n <- nrow(intervals_df)

  # Calculate neutrosophic mean using interval_div
  #mean_interval <- interval_div(sum_of_intervals, c(n, n))
  mean_interval <- interval_div(list(sum_of_intervals, c(n, n)))

  return(list(Neutrosophic_mean = mean_interval))
}



