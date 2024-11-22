#' Prepares a virtual dataset of parquet files to extract ICIO data
#' You can apply additional filters and do not forget to collect the data
#' @param folder Folder where the files are. In the server now: "E:/users/biedmlu/figaro_db/24ed/reference"
#' @param ic the type to query ("icsup","icuse","iciop","icioi"). By default "icsup"
#' @importFrom dplyr collect
#' @importFrom dplyr select
#' @importFrom arrow open_dataset
#' @return an arrow virtual dataset
#' @export fig_get_icio
#'
#' @examples
#' tmp <- fig_get_icio() |>
#' filter(REF_AREA == "ES" & OBS_VALUE !=0 & COUNTERPART_AREA==REF_AREA) |>
#' collect()
fig_get_icio <- function(folder = "E:/users/biedmlu/figaro_db/24ed/reference",
                         ic = "icsup") {
						 
tmp<-  arrow::open_dataset(list.files(path = folder,
                                 pattern = paste0("^",ic,"_"),
                                 full.names=TRUE)) |>
  dplyr::select(REF_AREA,COUNTERPART_AREA,ROW_PI,COL_PI,TIME_PERIOD,OBS_VALUE)
}



