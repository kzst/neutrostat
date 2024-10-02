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
interval_df <- function(data) {
  intervals <- list()
  for (i in 1:length(data)) {
    if (length(data[[i]]) == 1) {
      intervals[[i]] <- c(data[[i]], data[[i]])
    } else {
      intervals[[i]] <- data[[i]]
    }
  }
  df <- do.call(rbind, intervals)
  return(data.frame(First_Value = df[, 1], Second_Value = df[, 2]))
}
