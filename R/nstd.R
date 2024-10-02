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
# neutrosophic standard deviation computation
nstd <- function(data) {
  # Calculate neutrosophic variance using the existing nvar function
  var <- nvar(data)$Neutrosophic_variance

  # Calculate the square root of the variance interval
  if (var[1] < 0 || var[2] < 0) {
    stop("Square root of a negative interval is not defined")
  }
  std_var <- c(sqrt(var[1]), sqrt(var[2]))

  return(list(Neutrosophic_std = std_var))
}


