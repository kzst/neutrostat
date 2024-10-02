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
# A function to find summary of the neutrosophic data
nsummary <- function(data) {
  # Calculate neutrosophic mean
  mean_result <- nmean(data)$Neutrosophic_mean

  # Calculate neutrosophic median
  median_result <- nmedian(data)$Neutrosophic_median

  # Calculate neutrosophic standard deviation
  std_result <- nstd(data)$Neutrosophic_std

  # Calculate neutrosophic quantiles (Q1 and Q3)
  quantiles_result <- nquant(data)
  q1_result <- quantiles_result$Qu_1
  q3_result <- quantiles_result$Qu_3

  # Sort intervals to find min and max
  sorted_intervals <- interval_sort(data)
  min_value <- sorted_intervals[[1]]
  max_value <- sorted_intervals[[length(sorted_intervals)]]

  # Create a data frame with the results
  summary_df <- data.frame(
    Statistic = c("Neutro_min", "Neutro_mean", "Neutro_Q1", "Neutro_median", "Neutro_Q3", "Neutro_std", "Neutro_max"),
    Value = I(list(min_value, mean_result, q1_result, median_result, q3_result, std_result, max_value))
  )

  return(summary_df)
}



