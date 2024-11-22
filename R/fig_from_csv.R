
#' Read csv files with right datatable options for NA
#'
#' @param file file to read
#' @param ... other additional parameters
#'
#' @importFrom data.table fread
#' @return a data.table
#' @export fig_from_csv
#'

fig_from_csv <- function(file,...){
  data.table::fread(file,na.strings = c(""), fill = TRUE)
}

