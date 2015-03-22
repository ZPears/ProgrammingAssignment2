## The functions in cachematrix.R enable users to cache a symmetrical matrix and its inverse within a single object.
## This increases efficiency in programs where the inverse of matrices must be calculated frequently.

## Assigns several methods to an object (meant to be a square matrix). These methods allow you to:
## set - assign a new object to the variable. Issues warning if new object is not matrix, or not sqaure
## get - print the object stored in the variable
## set_inverse - assigns the inverse of the variable to the cached object "inverse"
## get_inverse - prints the value of the cached object "inverse"

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    if (!is.matrix(x)) {
      message("WARNING: Object must be a matrix to use all makeCacheMatrix methods")
    }
    if ((is.matrix(x)) && (!isSymmetric(x))) {
      message("WARNING: Matrix must be square to use all makeCacheMatrix methods")
    }
  }
  get <- function() {
    x
  }
  set_inverse <- function() {
    inverse <<- solve(x)
  }
  get_inverse <- function() {
    inverse
  }
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Returns the cached inverse of an object previously defined by the function makeCacheMatrix.
## If no inverse is cached, cacheSolve will assign one.
## If x is not a symmetrical matrix, cacheSolve will return an error.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("Retrieving cached data")
    return(inv)
  }
  else {
    inv <- x$set_inverse()
    return(inv)
  }
}
