#' Random Initialization
#' 
#' Randomly initialize weight matrices while implementing symmetry breaking.
#' @importFrom stats rnorm
#' @param L_in Positive integer equal to the number of neurons in the previous/incoming hidden layer.
#' @param L_out Positive integer equal to the number of neurons in the next/outgoing hidden layer.
#' @param epsilon.init Real number parameter for symmetry breaking.
#' @details If the value of epsilon is not specified, it takes on the default value of \code{sqrt(6)/sqrt(L_in + L_out)} since this is considered a suitable value for \eqn{\epsilon}.
#' @return A matrix of size \code{L_out by (1+L_in)}
#' @examples 
#' t <- rand_init(3,3,0.15)
#' th <- rand_init(4,5)
#' @export

#function to randomly initiate weights
rand_init=function(L_in,L_out,epsilon.init = (sqrt(6)/sqrt(L_in+L_out))){
  num=L_out*(1+L_in)
  vals=rnorm(num)
  W=(matrix(vals,L_out,(1+L_in))*2*epsilon.init)-epsilon.init
  return(W)
}
