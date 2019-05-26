#' Gradient of sigmoid function
#' 
#' Evaluate the gradient of the sigmoid function.
#' @param z Matrix/vector input
#' @inheritParams sigmoid
#' @details \code{sigmoidgradient} implements \code{sigmoid} function. The \code{sigmoidgradient} function is a significant part of the backpropagation part of the neural network algorithm.
#' @return Matrix with dimensions same as the input matrix.
#' @examples 
#' m <- matrix(c(1:60),4,15)
#' sg <- sigmoidgradient(m)
#' @export
sigmoidgradient=function(z){
  g = matrix(0,nrow(z),ncol(z))
  a = sigmoid(z)
  g = a*(1-a)
  return(g)
}
