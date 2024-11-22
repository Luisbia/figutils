
#' Extract NAMA data from MDT
#'Some columns are ignored and some filters applied to make it faster (only current prices). For very particular extractions use the normal procedure.
#' @param db_nama MDT table to query, default option is "NAMA_REFERENCE_25ED"
#' @param year_nama years to extract, by defaul is 2010:2023
#' @import RMdt
#' @import dplyr
#' @importFrom data.table as.data.table
#' @return a data.table
#' @export fig_get_nama
#'
#' @examples
#' dt <- fig_get_nama(db_nama = "NAMA_PRODUCTION_25ED", year_nama = 2023)
fig_get_nama <- function(db_nama = "NAMA_REFERENCE_25ED",
                    year_nama = seq(2010,2023,1L)){
library(RMdt)
library(dplyr)
ch <- connect("figaroProd")

dtNama <- tbl(ch, db_nama) %>%
  select(PRICES,PROD_STAGE, REF_AREA,COUNTERPART_AREA,REF_SECTOR,ACCOUNTING_ENTRY,STO,ACTIVITY,TIME_PERIOD,OBS_VALUE) |>
  filter(TIME_PERIOD %in% year_nama,
         PRICES == "V") %>%
  select(-PRICES) |>
  data.table::as.data.table() |>
janitor::clean_names()
  DBI::dbDisconnect(ch)
  return(dtNama)
}

