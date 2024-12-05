#' Apply a standard 2 dimension GRAS
#'
#' @param mPrior a matrix with the prior values
#' @param cTarget Column target
#' @param rTarget Row target
#' @param balancingError Tolerance factor (default=0.001)
#' @param maxIter Maximum number of iterations (default=500)
#' @param verbose Print iteration number and error at each iteration (default=TRUE)
#' @param printMatrix Print matrix at each iteration (default=FALSE)
#'
#' @return a matrix satisfiying that rowsums and colsums equals the row and column targets
#' @export fig_GRAS
#'
#' @examples
# B <- matrix(c(1, 3, 5, 8, 2, 3, -12, 2, -20, 6, 1, 2),
#             ncol = 3,
#             nrow = 4,
#             byrow = TRUE)
# u <- c(8, 12, -2, 10)
# v <- c(10, 12, 6)
#' #
# mPosterior <- fig_GRAS(mPrior = B, cTarget = u, rTarget = v, printMatrix = TRUE)
fig_GRAS <- function(mPrior,
                     cTarget,
                     rTarget,
                     balancingError = 1e-10,
                     maxIter = 500,
                     verbose = TRUE,
                     printMatrix = FALSE) {
  # Multipliers function
  allocNP <- function(vec, targetValue) {
    P <- vec
    P[which(vec <= 0)] <- 0
    N <- P - vec

    P <- sum(P)
    N <- sum(N)

    discriminant <- targetValue * targetValue - 4 * P * (-N)
    if (P == 0 & N != 0) {
      kp <- N / targetValue
      kn <- targetValue / N
    } else if (P != 0 & N == 0) {
      kp <- targetValue / P
      kn <- P / targetValue
    } else if ((P == 0 & N == 0) | targetValue == 0) {
      kp <- 0
      kn <- 0
    } else if (P != 0 & N != 0) {
      kp <- (targetValue + sqrt(discriminant)) / (2 * P)
      kn <- (targetValue - sqrt(discriminant)) / (2 * N)
    }

    postVec <- vec
    postVec[which(vec >= 0)] <- vec[which(vec >= 0)] * kp
    postVec[which(vec < 0)] <- -vec[which(vec < 0)] * kn
    postVec[is.nan(postVec)] <- 0.0
    return(postVec)
  }

  # Check for consistency of prior and targets dimensions
  cols <- dim(mPrior)[2]
  rows <- dim(mPrior)[1]

  if (cols != length(rTarget)) {
    stop(paste0(
      "Number of columns ", cols, " and the length of the column target ", length(rTarget),
      " are not equal!"
    ))
  }
  if (rows != length(cTarget)) {
    stop(paste0(
      "Number of rows ", rows, " and the length of the row target ", length(cTarget),
      " are not equal!"
    ))
  }

  cError <- tail(sort(rowSums(mPrior) - cTarget), n = 1)
  rError <- tail(sort(colSums(mPrior) - rTarget), n = 1)
  print(paste0("Max initial discrepancy rows:  ", names(cError), " = ", round(cError, 0)))
  print(paste0("Max initial discrepancy columns:  ", names(rError), " = ", round(rError, 0)))

  # Iteration parameters
  loopError <- as.numeric(max(cError, rError))
  iterNumber <- 1


  # Loop
  while (loopError > balancingError & iterNumber < maxIter) {
    if (printMatrix) {
      print(mPrior)
    }

    for (i in 1:nrow(mPrior)) mPrior[i, ] <- allocNP(mPrior[i, ], cTarget[i])
    for (j in 1:ncol(mPrior)) mPrior[, j] <- allocNP(mPrior[, j], rTarget[j])

    cError <- tail(sort(rowSums(mPrior) - cTarget), n = 1)
    rError <- tail(sort(colSums(mPrior) - rTarget), n = 1)

    if (verbose) {
      if (cError > rError) {
        reportLine <- paste0("Iter ", iterNumber, " max error: ", names(cError), " = ", round(as.numeric(cError), 3))
      } else {
        reportLine <- paste0("Max error: ", names(rError), " = ", round(as.numeric(rError), 3))
      }

      print(reportLine)
    }

    loopError <- max(cError, rError)
    iterNumber <- iterNumber + 1

    if (iterNumber > maxIter) {
      message(
        "Maximum number of iterations. Convergence was not achieved at ",
        iterNumber, " iterations. Error: ", loopError
      )
    }
  }

  if (loopError > balancingError) {
    message("convergence was not achieved at ", iterNumber, " iterations. Error: ", loopError)
  } else {
    message("converged at ", iterNumber, " iterations, with an error of ", loopError)
  }

  return(mPrior)
}
