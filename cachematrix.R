## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Set function create a matrix with Y numeric values, ROWS and COLS represents
## the matrix dimension. MINV represents the variable for the inverse matrix of X
makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL
  set <- function(y=numeric(), rows=numeric(), cols=numeric()) {
    x <<- matrix(y, nrow=rows, ncol=cols)
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(minverse) minv <<- minverse
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)

}


## Write a short comment describing this function

## Library MASS contains the function ginv(X) that calculates 
## the Moore-Penrose generalized inverse of a matrix X.

library("MASS") 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached inverse matrix")
    return(minv)
  }
  matrix_data <- x$get()
  minv <- ginv(matrix_data, ...)
  x$setminv(minv)
  minv
}
