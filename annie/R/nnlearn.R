#' Neural Network Training
#' 
#' Function to train a neural network.
#' @importFrom stats optim
#' @param x Matrix containing feature data to be supplied to the neural network for training.
#' @param y Vector containing label data corresponding to the rows of \code{x}.
#' @param layers Number of hidden layers in the neural network.
#' @param hidden.units Number of hidden units/neurons in each hidden layer.
#' @param lambda The regularization parameter. Must be a real number.
#' @param w Vector containing initial weights.
#' @param ... Further arguments passed to or from other methods.
#' @details Optimizes the weights by calling the \code{optim} function and passes \code{forwardpass} and \code{costfunctiongrad} as the function and gradient respectively. 
#' \itemize{
#'  \item If \code{method} is not specified, \code{optim} uses the \code{CG} as the default method.
#'  \item If \code{lambda} is unspecified, it assumes the default value of \code{0} and \code{nnlearn} performs unregularized training.
#'  \item If \code{w} is unspecified, \code{nnlearn} calls \code{initiate_theta} and creates a vector of weights and uses that newly created vector.
#' }
#' @return List containing the optimized weight matrices.
#' @examples 
#' data <- data.matrix(iris)
#' x <- data[,1:4]
#' y <- data[,5]
#' v <- c(1:39)/100
#' t <- nnlearn(x,y,layers = 2,hidden.units = 3,lambda = 0.85,w = v)
#' @export
nnlearn = function(x,y,layers,hidden.units,w = unlist(initiate_theta(x,y,layers,hidden.units)),lambda=0,...){
  outputs = unique(y)
  #number of unique outputs
  num.labels = length(unique(y))
  #dimensions of inputs matrix
  num.examples = nrow(x)
  num.features = ncol(x)
  #rand.wts = unlist(initiate_theta(x,y,layers,hidden.units))
  opt.wts = optim(par=w,fn=costfunction,x=x,y=y,layers=layers,hidden.units=hidden.units,lambda=lambda,gr=costfunctiongrad,method="CG")
  w.vec = opt.wts$par
  weights = list()
  start.idx = 1
  end.idx = hidden.units*(num.features+1)
  weights[[1]] = matrix(w.vec[start.idx:end.idx],hidden.units,(num.features+1))
  for (i in 2:layers){
    start.idx = start.idx + (nrow(weights[[i-1]])*ncol(weights[[i-1]]))
    end.idx = (start.idx-1) + (hidden.units*(hidden.units+1))
    weights[[i]] = matrix(w.vec[start.idx:end.idx],hidden.units,(hidden.units+1))
  }
  start.idx = start.idx + (nrow(weights[[layers]])*ncol(weights[[layers]]))
  end.idx = (start.idx-1) + (num.labels*(hidden.units+1))
  weights[[layers+1]] = matrix(w.vec[start.idx:end.idx],num.labels,(hidden.units+1))
  return(weights)
}
