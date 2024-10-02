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
# sorting of neutrosophic numbers
interval_sort <- function(data) {
  # Conversion data to intervals df
  intervals_df <- interval_df(data)

  # Sorting the df by 1_Value and then 2_Value
  sorted_df <- intervals_df[order(intervals_df$First_Value, intervals_df$Second_Value), ]

  # Conversion the sorted df back to a list of intervals
  sorted_intervals <- lapply(1:nrow(sorted_df), function(i) c(sorted_df$First_Value[i], sorted_df$Second_Value[i]))

  return(sorted_intervals)
}

