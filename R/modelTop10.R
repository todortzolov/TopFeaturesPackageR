
#write out code and documeentation using #' format, then go to Build tab in RStudio


#' Building a Model with Top Ten Features
#'
#' This function develops a prediction algorithm based on the top 10 features
#' in 'x' that are most predictive of 'y'.
#'
#' @param x a n x p matrix of n observations and p predictors
#' @param y a vector of length n representing the response
#' @return a 'lm' object representing the linear model with the top 10 predictors
#' @author Todor
#' @details
#' This function runs a univariate regression of y on each predictor in x and
#' calculates the p-value indicating the significance of the association. The
#' final set of 10 predictors is the taken from the 10 smallest p-values.
#' @seealso \code{lm}
#' @import stats
#this imports outside functions used
#' @export
#this exports the function for use
topten <- function(x, y) {
  p <- ncol(x)
  if(p < 10)
    stop("there are less than 10 predictors - check if there are at least 10")
  pvalues <- numeric(p) #initialize empty vector of p values
  for(i in seq_len(p)) { #loop through each predictor and fit univariate prediction model
    fit <- lm(y ~ x[, i])
    summ <- summary(fit) #get summary of p values
    pvalues[i] <- summ$coefficients[2, 4] #accumulate p values only, which is 4th column in coefficients element
  }
  ord <- order(pvalues) # sort the p values using indexes from smallest to largest (default)
  ord <- ord[1:10] #take only the top 10
  x10 <- x[, ord]# create a new dataset from the original
  fit <- lm(y ~ x10) #create new model
  coef(fit) #grab coefficients from the model
}

#' Prediction with Top Ten Features
#takes a matrix of predictors X and vector coefficients b

#' Prediction with Top Ten Features
#'
#' This function takes a set coefficients produced by the \code{topten}
#' function and makes a prediction for each of the values provided in the
#' input 'X' matrix.
#'
#' @param X a n x 10 matrix containing n observations
#' @param b a vector of coefficients obtained from the \code{topten} function
#' @return a numeric vector containing the predicted values
predict10 <- function(X, b) {
  X <- cbind(1, X) #add intercept
  drop(X %*% b) #matrix multiplication of the new data and coefficient for the model X *b
}
