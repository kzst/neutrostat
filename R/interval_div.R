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

# Generalized function to find division of two neutrosophic numbers
interval_div <- function(data) {

  intervals_df <- interval_df(data)


  intervals <- list()
  for (i in 1:nrow(intervals_df)) {
    intervals[[i]] <- c(intervals_df$First_Value[i], intervals_df$Second_Value[i])
  }


  if (length(intervals) < 2) {
    stop("At least two intervals are required")
  }

  interval_div_inner <- function(interval1, interval2) {
    if (interval2[1] == 0 || interval2[2] == 0) {
      stop("Division by zero interval is not defined")
    }
    reciprocal <- c(1 / interval2[2], 1 / interval2[1])
    return(interval_mul(list(interval1, reciprocal)))
  }

  result <- intervals[[1]]
  for (i in 2:length(intervals)) {
    result <- interval_div_inner(result, intervals[[i]])
  }
  return(result)
}


