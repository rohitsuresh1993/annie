% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initiate_theta.R
\name{initiate_theta}
\alias{initiate_theta}
\title{Randomly initialize weights}
\usage{
initiate_theta(x, y, layers, hidden.units, ...)
}
\arguments{
\item{x}{Matrix containing feature data to be supplied to the neural network for training.}

\item{y}{Vector containing label data corresponding to the rows of \code{x}.}

\item{layers}{Number of hidden layers in the neural network.}

\item{hidden.units}{Number of hidden units/neurons in each hidden layer.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
List of length \code{layers+1} containing randomly initialized weight matrices.
}
\description{
Randomly initialize the weight matrices for a neural network.
}
\details{
\code{initiate_theta} implements \code{rand_init} to generate weight matrices.
}
\examples{
y <- data.matrix(iris)[1:100,5]
x <- data.matrix(iris)[101:150,1:4]
w <- initiate_theta(x,y,2,3)
}
