#' Create a time stamp for Figaro last_update
#'
#' @return a formatted time stamp
#' @export fig_time
#'
fig_time <- function() {
  as.character(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
  }
