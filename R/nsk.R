
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
# Product Moment Coefficient of Skewness
nsk <- function(data) {
  mean_interval <- nmean(data)$Neutrosophic_mean
  std_interval <- nstd(data)$Neutrosophic_std

  # Calculate third central moment
  IDF <- interval_df(data)
  intervals_list <- lapply(1:nrow(IDF), function(i) c(IDF$First_Value[i], IDF$Second_Value[i]))
  n <- nrow(IDF)

  moment_3 <- lapply(intervals_list, function(interval) {
    l_diff <- interval[1] - mean_interval[1]
    u_diff <- interval[2] - mean_interval[2]
    c(l_diff^3, u_diff^3)
  })

  moment_3_sum <-interval_add(moment_3)
  nmoment_3 <- interval_div(list(moment_3_sum, c(n, n)))

  # Calculate skewness using interval_div and interval_mul
  sk <- interval_div(list(nmoment_3, interval_mul(list(std_interval, interval_mul(list(std_interval, std_interval))))))

  return(list(Neutrosophic_Skewness = sk))
}

