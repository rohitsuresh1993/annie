#' Diagnostics of neural network
#' 
#' Evaluate performance parameters of a neural network.
#' @param predictions Vector of labels typically obtained as output of \code{predict} function.
#' @param y.test Vector of labels typically from the testing dataset.
#' @details Calculates accuracy and calculates F1 score by evaluating precision and recall. \code{predications} and \code{y.test} must be vectors of same length.
#' @return A list containing
#' \itemize{
#'  \item \code{Accuracy}
#'  \item \code{F1.scores}
#' }
#' @examples
#' y <- data.matrix(iris)[1:100,5]
#' x <- data.matrix(iris)[101:150,1:4]
#' t <- nnlearn(x,y,layers = 2,hidden.units = 3,lambda = 0.85)
#' p <- nnpredict(t,x,y)
#' y.test <- data.matrix(iris)[101:150,5]
#' D <- diagnostics(p,y.test)
#' @export
diagnostics = function(predictions,y.test){
  r = list()
  r$Accuracy = 100*sum(predictions==y.test)/length(y.test)
  outputs = unique(y.test)
  l = length(outputs)
  F1 = rep(0,l)
  for (i in 1:l){
  tp = sum(predictions==outputs[i] & y.test==outputs[i])
  tn = sum(predictions!=outputs[i] & y.test!=outputs[i])
  fp = sum(predictions==outputs[i] & y.test!=outputs[i])
  fn = sum(predictions!=outputs[i] & y.test==outputs[i])
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  if (precision == 0 & recall == 0 | is.na(precision | is.na(recall))){
    F1[i] = 0
  }else{
    F1[i] = 2*precision*recall / (precision+recall)
  }
  }
  r$F1.scores = F1
  return(r)
}
