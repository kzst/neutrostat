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
# Function to calculate coefficient of variation of the neutrosophic data
ncv <- function(data) {
  # neutrosophic mean
  mean_interval <- nmean(data)$Neutrosophic_mean

  # neutrosophic standard deviation
  std_interval <- nstd(data)$Neutrosophic_std

  # To ensure neutrosophic mean is not zero
  if (mean_interval[1] == 0 || mean_interval[2] == 0) {
    stop("Coefficient of variation is not defined when the mean interval contains zero.")
  }

  # Required ratio
  cv <- interval_div(list(std_interval, mean_interval))

  return(list(Neutrosophic_CV = cv*100))
}

