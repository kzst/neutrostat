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
# Product Moment Coefficient of Kurtosis
nkur <- function(data) {

  mean_interval <- nmean(data)$Neutrosophic_mean

  std_interval <- nstd(data)$Neutrosophic_std

  # Calculate fourth central moment
  IDF <- interval_df(data)
  intervals_list <- lapply(1:nrow(IDF), function(i) c(IDF$First_Value[i], IDF$Second_Value[i]))
  n <- nrow(IDF)

  moment_4 <- lapply(intervals_list, function(interval) {
    l_diff <- interval[1] - mean_interval[1]
    u_diff <- interval[2] - mean_interval[2]
    c(l_diff^4, u_diff^4)
  })

  moment_4_sum <-interval_add(moment_4)
  nmoment_4 <- interval_div(list(moment_4_sum, c(n, n)))

  # Calculate kurtosis using interval_div and interval_mul
  std_sq <- interval_mul(list(std_interval, std_interval))
  std_quad <- interval_mul(list(std_sq, std_sq))

  kur <- interval_div(list(nmoment_4, std_quad))

  return(list(Neutrosophic_Kurtosis = kur))
}
