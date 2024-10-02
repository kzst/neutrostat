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
interval_sub <- function(data) {

  intervals_df <- interval_df(data)


  intervals <- list()
  for (i in 1:nrow(intervals_df)) {
    intervals[[i]] <- c(intervals_df$First_Value[i], intervals_df$Second_Value[i])
  }


  if (length(intervals) < 2) {
    stop("At least two intervals are required")
  }


  output <- intervals[[1]]


  for (i in 2:length(intervals)) {
    output <- c(output[1] - intervals[[i]][2], output[2] - intervals[[i]][1])
  }

  return(output)
}

