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
# neutrosophic variance computation
nvar <- function(data) {
  # Convert data to intervals dataframe using the existing function
  intervals_df <- interval_df(data)

  # Convert dataframe to list of intervals
  intervals_list <- lapply(1:nrow(intervals_df), function(i) c(intervals_df$First_Value[i], intervals_df$Second_Value[i]))

  # Calculate neutrosophic mean using the existing function
  mean_interval <- nmean(data)$Neutrosophic_mean

  # Number of intervals
  n <- nrow(intervals_df)

  # Calculate the deviations from the mean
  D <- lapply(intervals_list, function(interval) interval_sub(list(interval, mean_interval)))

  # Calculate the squared deviations
  SD <- lapply(D, function(deviation) interval_mul(list(deviation, deviation)))

  # Sum the squared deviations
  SSD <- interval_add(SD)

  # Calculate the variance
  var <- interval_div(list(SSD, c(n, n)))

  return(list(Neutrosophic_variance = var))
}



