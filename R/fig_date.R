#' Create a date stamp for Figaro output files
#'
#' @return a formatted time stamp
#' @export fig_date
#'
fig_date <- function() {
  as.character(format(Sys.time(), "%Y%m%d"))
  }

