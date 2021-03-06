% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forwardpass.R
\name{forwardpass}
\alias{forwardpass}
\title{Forward pass and Backpropagation}
\usage{
forwardpass(x, y, layers, hidden.units, lambda, th.vec, ...)
}
\arguments{
\item{x}{Matrix containing feature data to be supplied to the neural network for training.}

\item{y}{Vector containing label data corresponding to the rows of \code{x}.}

\item{layers}{Number of hidden layers in the neural network.}

\item{hidden.units}{Number of hidden units/neurons in each hidden layer.}

\item{lambda}{The regularization parameter. Must be a real number.}

\item{th.vec}{Vector containing initial weights.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A list containing
\describe{
 \item{Hypothesis}{Matrix whose rows contain to the probabilities of the label being equal to the corresponding output.}
 \item{\code{Cost}}{Value of cost function calculated with the supplied vector of weights.}
 \item{\code{D}}{Vector containing partial derivatives of the cost function with respect to each weight.}
}
}
\description{
Implements the neural network with an initial set of weights, calculates the error and backpropagates the error through the neural network.
}
\details{
\code{forwardpass} implemets the neural network and backpropagation based on the inputs and parameters supplied by the user. 
It calculates the cost, cost gradient and the hypothesis of the neural network. \code{forwardpass} does not optimize the weights.
}
\examples{
data <- data.matrix(iris)
x <- data[,1:4]
y <- data[,5]
w <- c(1:39)/100
h <- forwardpass(x,y,layers = 2,hidden.units = 3,lambda = 0.85,th.vec = w)
}
