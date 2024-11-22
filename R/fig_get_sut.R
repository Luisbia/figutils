
#' Extract SUT data from MDT
#'Some columns are ignored and some filters applied to make it faster (only current prices). For very particular extractions use the normal procedure.
#' @param db_sut MDT table to query, default option is "SUT_REFERENCE_25ED"
#' @param year_sut years to extract, by defaul is 2010:2023
#' @param geo_sut countries to extract. A value is required
#' @param tables_sut tables to extract by default c("T1500","T1600","T1611","T1612","T1620","T1630")
#' @import RMdt
#' @import dplyr
#' @importFrom data.table as.data.table
#' @return a data.table
#' @export fig_get_sut
#'
#' @examples
#' dt <- fig_get_sut(db_sut = "SUT_PRODUCTION_25ED", year_sut = 2021, geo_sut = "ES")
fig_get_sut <- function(db_sut = "SUT_REFERENCE_25ED",
                        year_sut = seq(2010,2023,1L),
                        tables_sut = c("T1500", "T1600","T1611","T1612","T1620","T1630"),
                        geo_sut){
  library(RMdt)
  library(dplyr)
  ch <- connect("figaroProd")

  dtsut <- tbl(ch, db_sut) %>%
    select(TABLE_IDENTIFIER,PRICES,PROD_STAGE, REF_AREA,PRICES,ROW_PI,COL_PI,TIME_PERIOD,OBS_VALUE) |>
    filter(TIME_PERIOD %in% year_sut,
           TABLE_IDENTIFIER %in% tables_sut,
           PRICES == "V") %>%
    select(-PRICES) |>
    data.table::as.data.table() |>
    janitor::clean_names()
  DBI::dbDisconnect(ch)
  return(dtsut)
}



