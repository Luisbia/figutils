#' GRAS method
#'
#' A detailed explanation is available here: https://kamecon.quarto.pub/ras/#the-allocnp-function-figaro-implementation-of-gras
#'
#' @param vec a vector, row or column of a matrix of initial values
#' @param targetValue a target value for the sum of the elements of the vector, row or column
#'
#' @return a vector whose sum is equal to the target value
#' @export fig_allocNP
#'
fig_allocNP <- function(vec, targetValue){
  P <- vec
  P[which(vec <= 0)] <- 0
  N <- P - vec

  P <- sum(P)
  N <- sum(N)

  discriminant = targetValue*targetValue - 4 * P*(-N)
  if(P == 0 & N != 0){
    kp <- N / targetValue
    kn <- targetValue/ N
  } else if(P != 0 & N == 0){
    kp <- targetValue / P
    kn <- P / targetValue
  } else if((P == 0 & N == 0) | targetValue == 0){
    kp <- 0
    kn <- 0
  } else if(P != 0 & N != 0){
    kp <- (targetValue + sqrt(discriminant)) / (2*P)
    kn <- (targetValue - sqrt(discriminant)) / (2*N)
  }

  postVec <- vec
  postVec[which(vec >= 0)] <- vec[which(vec >= 0)]*kp
  postVec[which(vec < 0)] <- -vec[which(vec < 0)]*kn
  postVec[is.nan(postVec)] <- 0.0
  return(postVec)
}
