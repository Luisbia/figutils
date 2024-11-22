#' Create a time and date stamp for Figaro output files
#'
#' @return a formatted time stamp
#' @export fig_date_time
#'
fig_date_time <- function() {
  as.character(format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
