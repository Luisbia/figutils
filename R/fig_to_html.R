#' Create an HTML interactive table with DT
#'
#' @description
#'
#' An interactive HTML table with some fancy options (filter,sort,export to excel) is generated from a data frame.
#'
#' @param x a data frame
#' @importFrom DT datatable
#' @return DT table
#' @export fig_to_hml
#'
#' @examples
#'
#' show_DT(mtcars)
#'
fig_to_html <- function(x){

  DT::datatable(x, filter = "top", class = "stripe hover", extensions = "Buttons",
                options = list(  lengthMenu = list(c(20, -1), c("20", "All")),
                                 pageLength = 20, dom = "Blfrtip", buttons = c("excel"))
  )}
