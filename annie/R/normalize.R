#' Normalize
#' 
#' Normalize skewed data before training neural network.
#' @importFrom stats sd
#' @param x Matrix containing feature data.
#' @details Normalizes data to have zero mean by subtracting column-wise mean from data and the scales data by dividing with standard deviation.
#' \deqn{x_{norm} = \frac{x-\mu_{x}}{\sigma_{x}}}{xnorm = (x-\mu)/\sigma}
#' @return  Normalized form of the input matrix with the same dimensions.
#' @examples 
#' x <- data.matrix(iris)[,1:4]
#' x.norm <- normalize(x)
#' @export
normalize = function(x){
  mean = apply(x,2,mean)
  std.dev = apply(x,2,sd)
  f = function(x)(x-mean)/std.dev
  norm.x=apply(x,2,f)
  return(norm.x)
}
