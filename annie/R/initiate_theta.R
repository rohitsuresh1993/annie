#' Randomly initialize weights
#' 
#' Randomly initialize the weight matrices for a neural network.
#' @param x Matrix containing feature data to be supplied to the neural network for training.
#' @param y Vector containing label data corresponding to the rows of \code{x}.
#' @param layers Number of hidden layers in the neural network.
#' @param hidden.units Number of hidden units/neurons in each hidden layer.
#' @details \code{initiate_theta} implements \code{rand_init} to generate weight matrices.
#' @return List of length \code{layers+1} containing randomly initialized weight matrices.
#' @inheritParams rand_init
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' y <- data.matrix(iris)[1:100,5]
#' x <- data.matrix(iris)[101:150,1:4]
#' w <- initiate_theta(x,y,2,3)
#' @export

initiate_theta=function(x,y,layers,hidden.units,...){
  #number of outputs
  num.labels=length(unique(y))
  #number of samples provided
  num.examples=nrow(x)
  #number of features in each example/sample
  ip.layer.size=ncol(x)
  #creating an empty list to store theta matrices
  theta=list()
  #theta for input layer and first hidden layer
  theta[[1]]=rand_init(ip.layer.size,hidden.units)
  #theta for last hidden layer and output layer
  theta[[layers+1]]=rand_init(hidden.units,num.labels)
  #thetas between hidden layers
  for (i in 2:layers){
    theta[[i]]=rand_init(hidden.units,hidden.units)
  }
  return(theta)
}
