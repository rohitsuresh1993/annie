#' Forward pass and Backpropagation
#' 
#' Implements the neural network with an initial set of weights, calculates the error and backpropagates the error through the neural network.
#' @param x Matrix containing feature data to be supplied to the neural network for training.
#' @param y Vector containing label data corresponding to the rows of \code{x}.
#' @param layers Number of hidden layers in the neural network.
#' @param hidden.units Number of hidden units/neurons in each hidden layer.
#' @param lambda The regularization parameter. Must be a real number.
#' @param th.vec Vector containing initial weights.
#' @param ... Further arguments passed to or from other methods.
#' @details \code{forwardpass} implemets the neural network and backpropagation based on the inputs and parameters supplied by the user. 
#' It calculates the cost, cost gradient and the hypothesis of the neural network. \code{forwardpass} does not optimize the weights.
#' @return A list containing
#' \describe{
#'  \item{Hypothesis}{Matrix whose rows contain to the probabilities of the label being equal to the corresponding output.}
#'  \item{\code{Cost}}{Value of cost function calculated with the supplied vector of weights.}
#'  \item{\code{D}}{Vector containing partial derivatives of the cost function with respect to each weight.}
#' }
#' @examples 
#' data <- data.matrix(iris)
#' x <- data[,1:4]
#' y <- data[,5]
#' w <- c(1:39)/100
#' h <- forwardpass(x,y,layers = 2,hidden.units = 3,lambda = 0.85,th.vec = w)
#' @export
forwardpass = function(x,y,layers,hidden.units,lambda,th.vec,...){
  #require(MASS)
  return.list = list()
  #vector of unique outputs
  outputs = unique(y)
  #number of unique outputs
  num.labels = length(unique(y))
  #dimensions of inputs matrix
  num.examples = nrow(x)
  num.features = ncol(x)
  #converting each example output into 1-0 vectors
  I = diag(num.labels)
  Y = matrix(0,num.examples,num.labels)
  for (i in 1:num.examples){
    Y[i,] = I[match(y[i],outputs),]
  }
  th = list()
  start.idx = 1
  end.idx = hidden.units*(num.features+1)
  th[[1]] = matrix(th.vec[start.idx:end.idx],hidden.units,(num.features+1))
  for (i in 2:layers){
    start.idx = start.idx + (nrow(th[[i-1]])*ncol(th[[i-1]]))
    end.idx = (start.idx-1) + (hidden.units*(hidden.units+1))
    th[[i]] = matrix(th.vec[start.idx:end.idx],hidden.units,(hidden.units+1))
  }
  start.idx = start.idx + (nrow(th[[layers]])*ncol(th[[layers]]))
  end.idx = (start.idx-1) + (num.labels*(hidden.units+1))
  th[[layers+1]] = matrix(th.vec[start.idx:end.idx],num.labels,(hidden.units+1))
  #forward pass
  a = list()
  z = list()
  a[[1]] = cbind(rep(1,num.examples),x)
  for (i in 1:(layers+1)){
      z[[i]] = a[[i]]%*%t(th[[i]])
      a[[i+1]] = cbind(rep(1,num.examples),sigmoid(z[[i]]))
  }
  h = a[[layers+2]][,-1]
  return.list$Hypothesis = h
  
  #backpropagation
  #calculating penalty
  pen = 0
  for (i in 1:length(th)){
    m = th[[i]][,2:ncol(th[[i]])]^2
    s = sum(apply(m,1,sum))
    pen = pen + s
  }
  #calculating cost function
  mtx = (-Y)*log(h) - (1-Y)*log(1-h)
  inner.sum = apply(mtx,1,sum)
  outer.sum = sum(inner.sum)
  J = outer.sum/num.examples + (lambda*pen/(2*num.examples))
  return.list$Cost = J
  #backpropagating error
  sigma = list()
  sigma[[layers+2]] = a[[layers+2]][,-1] - Y #[,-1] removes the columns of 1 added to a
  for (i in (layers+1):2){
    sgmtx = cbind(rep(1,nrow(z[[i-1]])),z[[i-1]])
    sigma[[i]] = (sigma[[i+1]]%*%th[[i]])*(sigmoidgradient(sgmtx))
    sigma[[i]] = sigma[[i]][,-1]
  }
  delta = list()
  reg.grad = list()
  D = list()
  for (i in 1:(layers+1)){
    delta[[i]] = t(sigma[[i+1]])%*%a[[i]]
    mtx = cbind(rep(0,nrow(th[[i]])),th[[i]][,-1])
    reg.grad[[i]] = (lambda/num.examples)*mtx
    D[[i]] = (delta[[i]]/num.examples)+reg.grad[[i]]
  }
  return.list$D = unlist(D)
  return(return.list)
}
