#' Extract ICIO table
#'
#' Extract basic matrices from source data and prepares it for
#' processing with `make_wio`
#' @param path Character string for source folder of zip file
#' @param year Integer
#' @param quiet Boolean, if `TRUE`, suppress all status messages. Default
#'   is `FALSE`, i.e., messages are shown.
#' @keywords internal
#' @noRd
#' @return List with basic input-output matrices and metadata
fig_extract_matrices_oecd_icio <- function(path,
                         year = NULL, quiet = FALSE) {
  
  # Dimensions and file names
  # ************
  # ICIO 2023
  # ************
  G <- 77
  GX <- 77
  N <- 45
  FD <- 6
  csv_file <- paste0(path,year, "_SML.csv")
  # Names of rows and columns
  g_names <- c("ARG", "AUS", "AUT", "BEL", "BGD", "BGR", "BLR", "BRA",
               "BRN", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COL",
               "CRI", "CYP", "CZE", "DEU", "DNK", "EGY", "ESP", "EST",
               "FIN", "FRA", "GBR", "GRC", "HKG", "HRV", "HUN", "IDN",
               "IND", "IRL", "ISL", "ISR", "ITA", "JOR", "JPN", "KAZ",
               "KHM", "KOR", "LAO", "LTU", "LUX", "LVA", "MAR", "MEX",
               "MLT", "MMR", "MYS", "NGA", "NLD", "NOR", "NZL", "PAK",
               "PER", "PHL", "POL", "PRT", "ROU", "RUS", "SAU", "SEN",
               "SGP", "SVK", "SVN", "SWE", "THA", "TUN", "TUR", "TWN",
               "UKR", "USA", "VNM", "ZAF", "ROW")
  gx_names <- g_names
  n_names <- c("D01T02", "D03", "D05T06", "D07T08", "D09", "D10T12",
               "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22",
               "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30",
               "D31T33", "D35", "D36T39", "D41T43", "D45T47", "D49",
               "D50", "D51", "D52", "D53", "D55T56", "D58T60",
               "D61", "D62T63", "D64T66", "D68", "D69T75", "D77T82",
               "D84", "D85", "D86T88", "D90T93", "D94T96", "D97T98")
  fd_names <- c("HFCE", "NPISH", "GGFC", "GFCF", "INVNT", "DIRPA")
  
  # Derived dimensions and names
  GN <- G * N
  GXN <- GX * N
  # FD <- 6
  GFD <- G * FD
  gn_names <- paste0(rep(g_names, each = N), gsub("[C|D]", "_", n_names))
  gxn_names <- paste0(rep(gx_names, each = N), gsub("[C|D]", "_", n_names))
  gfd_names <- paste0(rep(g_names, each = FD), "_", fd_names)
  
  # Extract data
  df <- data.table::fread(csv_file, stringsAsFactors = FALSE)
  
  # Get row names
  rowx_names <- as.character(df[[1]])
  
  # Remove first column of names
  df <- df[, -1]
  
  # Convert to matrix
  df <- as.matrix(df)
  
  # Get column names
  colx_names <- colnames(df)
  
  
  # Basic matrices: Z, Y, X, VA
  if (!quiet) { cli::cli_alert_info("Getting matrices Z, Y, X")}
  
  Z <- as.matrix(df[1:GXN, 1:GXN])
  rownames(Z) <- gxn_names
  
  # Y with FD components
  Yfd <- as.matrix(df[1:GXN, (GXN + 1):(GXN + GFD)])
  rownames(Yfd) <- gxn_names
  
  
  # Aggregation of Yfd
  Y <- matrix(0, GXN, G)
  for(r in 1:G) {
    p <- (r - 1)*FD + 1
    q <- (r - 1)*FD + FD
    # Check case FD = 1
    if (p == q) {
      Y[, r] <- Yfd[, p]
    } else {
      Y[, r] <- rowSums(Yfd[, p:q])
    }
    
  }
  rownames(Y) <- gxn_names
  colnames(Y) <- g_names
  
  # X and VA
  X <- as.numeric(rowSums(Z) + rowSums(Y))
  VA <- as.numeric(X - colSums(Z))
  names(X) <- names(VA) <- gxn_names
  
  
  
  # Name all matrices
  rownames(Z) <- colnames(Z) <- rownames(Y) <- rownames(Yfd) <- gxn_names
  colnames(Y) <- g_names
  colnames(Yfd) <- gfd_names
  names(VA) <- names(X) <- gxn_names
  
  # Create object io
  io <- list(Z, Yfd, Y, VA, X)
  names(io) <- c("Z", "Yfd", "Y", "VA", "X")
  
  # Metadata: dims
  io$dims <- list(G, N, FD, GX, GN, GXN, GFD)
  names(io$dims) <- c("G","N","FD", "GX","GN","GXN", "GFD")
  
  # Metadata: names
  io$names <- list(g_names, n_names, fd_names, gx_names,
                   gn_names, gxn_names, gfd_names)
  names(io$names) <- c("g_names","n_names","fd_names",
                       "gx_names", "gn_names", "gxn_names",
                       "gfd_names")
  
  
  io$year <- year
  
  return(io)
  
}