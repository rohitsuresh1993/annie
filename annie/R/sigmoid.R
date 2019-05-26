#' Sigmoid function
#' 
#' Evaluate element-wise sigmoid function of a matrix.
#' @param m Matrix or vector input.
#' @details Sigmoid function is the default activation function contained in each neuron of the neural network.
#' \deqn{sigmoid(m) = \frac{1}{1+e^{-m}}}{sigmoid(m) = 1/(1+exp(-m))}
#' The \code{sigmoid} function will be used very prominently in the forward pass of the neural networks algorithm.
#' @return Matrix with dimensions same as the input matrix/vector.
#' @examples 
#' m <- c(1:30)
#' s <- sigmoid(m)
#' x <- matrix(c(1:45),15,3)
#' y <- sigmoid(x)
#' @export
sigmoid = function(m){
  s=1/(1+exp(-m))
  return(s)
}
