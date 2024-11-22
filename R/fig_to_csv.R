
#' Write csv files with right datatable options for dates
#'
#' @param data a data frame/data.table/tibble
#' @param ... destination and other optional parameters
#' @importFrom data.table fwrite
#' @return a file in the destination provided
#' @export fig_to_csv
#'
#' @examples
#' fig_to_csv(mtcars,"D:/test.csv")

fig_to_csv <- function(data,...){
    data.table::fwrite(data,...,dateTimeAs = "write.csv")
}

