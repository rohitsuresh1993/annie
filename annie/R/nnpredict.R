#' Predict labels
#' 
#' Predict labels for new feature data.
#' @param weights List of weight matrices. Typically obtained as the output of \code{nnlearn} function.
#' @param x.new Matrix of feature data typically from the testing dataset.
#' @param y.train Vector of labels typically from the training dataset.
#' @return Vector containing predicted labels corresponding to rows of \code{x.new}.
#' @examples 
#' y <- data.matrix(iris)[1:100,5]
#' x <- data.matrix(iris)[101:150,1:4]
#' t <- nnlearn(x,y,layers = 2,hidden.units = 3,lambda = 0.85)
#' p <- nnpredict(t,x,y)
#' @export
nnpredict=function(weights,x.new,y.train){
  #vector of unique outputs
  x = x.new
  y = y.train
  outputs = unique(y)
  #number of unique outputs
  num.labels = length(unique(y))
  #dimensions of inputs matrix
  num.inputs = nrow(x)
  num.features = ncol(x)
  num.weights = length(weights)
  h = list()
  m = cbind(rep(1,nrow(x)),x)
  h[[1]] = sigmoid(m %*% t(weights[[1]]))
  for (i in 2:num.weights){
    m = cbind(rep(1,nrow(h[[i-1]])),h[[i-1]])
    h[[i]] = sigmoid(m %*% t(weights[[i]]))
  }
  hyp = h[[num.weights]]
  maxs = apply(hyp,1,max)
  c = col(hyp)
  a = c[hyp==maxs]
  pred = outputs[a]
  return(pred)
}
