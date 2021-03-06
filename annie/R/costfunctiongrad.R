#' Gradient of cost function
#' 
#' Gradient of cost function evaluated with respect to each weight.
#' @param x Matrix containing feature data to be supplied to the neural network for training.
#' @param y Vector containing label data corresponding to the rows of \code{x}.
#' @param layers Number of hidden layers in the neural network.
#' @param hidden.units Number of hidden units/neurons in each hidden layer.
#' @param lambda The regularization parameter. Must be a real number.
#' @param th.vec Vector containing initial weights.
#' @param ... Further arguments passed to or from other methods.
#' @details Calls the \code{forwardpass} function and isolates the \code{D} to be used during optimization.
#' @return Vector containing partial derivatives of the cost function w.r.t each weight parameter.
#' @examples 
#' data <- data.matrix(iris)
#' x <- data[,1:4]
#' y <- data[,5]
#' w <- c(-28:27)/100
#' J <- costfunctiongrad(x,y,layers = 2,hidden.units = 3,lambda = 0.85,th.vec = w)
#' J1 <- costfunctiongrad(x,y,2,3,0.54,w)
#' @export
costfunctiongrad = function(x,y,layers,hidden.units,lambda,th.vec,...){
  hyp = forwardpass(x,y,layers,hidden.units,lambda,th.vec)
  return(hyp$D)
}
