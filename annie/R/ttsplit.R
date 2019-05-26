#' Test-Train split
#' 
#' Split the data into training and testing datasets.
#' @param x Matrix containing feature data.
#' @param y Vector of labels corresponding to feature data contained in \code{x}. \code{y} must be the same length as \code{x}.
#' @param fraction Real number between 0 and 1.
#' @details Function samples random indices and creates sub datasets for training and testing from the original complete dataset.
#' If the value of \code{fraction} in unspecified, it takes on the default value 0.66.
#' @return A list containing
#' \describe{
#'  \item{\code{x.train}}{Feature data to be used for training.}
#'  \item{\code{y.train}}{Labels corresponding to \code{x.train}.}
#'  \item{\code{x.test}}{Feature data to be used for testing.}
#'  \item{\code{y.test}}{Labels corresponding to \code{x.test}.} 
#' }
#' @examples 
#' data <- data.matrix(iris)
#' x <- data[,1:4]
#' y <- data[,5]
#' nn.data <- ttsplit(x,y)
#' nn.data <- ttsplit(x,y,fraction = 0.70)
#' @export

ttsplit = function(x,y,fraction = 0.66){
  m = nrow(x)
  num.train = ceiling(fraction*m)
  train.indices = sample(c(1:m),num.train)
  r=list()
  r$x.train = x[train.indices,]
  r$x.test = x[-train.indices,]
  r$y.train = y[train.indices]
  r$y.test = y[-train.indices]
  return(r)
}
