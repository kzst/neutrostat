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
# Generalized function to calculate the product of the neutrosophic numbers
interval_mul <- function(data) {

  intervals_df <- interval_df(data)


  intervals <- list()
  for (i in 1:nrow(intervals_df)) {
    intervals[[i]] <- c(intervals_df$First_Value[i], intervals_df$Second_Value[i])
  }


  if (length(intervals) < 2) {
    stop("At least two intervals are required")
  }

  interval_mul_inner <- function(interval1, interval2) {
    a1 <- interval1[1]
    b1 <- interval1[2]
    a2 <- interval2[1]
    b2 <- interval2[2]

    if (a1 >= 0 && a2 >= 0) {
      return(c(a1 * a2, b1 * b2))
    } else if (b1 < 0 && a2 >= 0) {
      return(c(a1 * b2, a2 * b1))
    } else if (b1 <= 0 && b2 <= 0) {
      return(c(b1 * b2, a2 * a1))
    } else if (b1 < 0 && b2 < 0) {
      return(c(b1 * b2, a1 * a2))
    } else if (a1 < 0 && b1 >= 0 && a2 < 0 && b2 >= 0) {
      return(c(min(abs(a1 * b2), abs(b1 * a2)), max(a1 * a2, b1 * b2)))
    } else if (a1 >= 0 && b2 < 0) {
      return(c(a2 * b1, b2 * a1))
    } else {
      alpha <- min(a1 * a2, a1 * b2, b1 * a2, b1 * b2)
      beta <- max(a1 * a2, a1 * b2, b1 * a2, b1 * b2)
      return(c(alpha, beta))
    }
  }

  result <- intervals[[1]]
  for (i in 2:length(intervals)) {
    result <- interval_mul_inner(result, intervals[[i]])
  }
  return(result)
}

