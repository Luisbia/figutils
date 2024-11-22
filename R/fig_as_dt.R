
#' Convert a vector or matrix to a data.table
#'
#' @param input a vector or a matrix
#'
#' @importFrom data.table as.data.table
#' @return a data.table
#' @export fig_as_dt
#'

fig_as_dt <- function(input){

  output <- data.table::as.data.table(input,
                                      keep.rownames = TRUE)
  return(output)
}
