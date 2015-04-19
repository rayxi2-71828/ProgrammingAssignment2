## Put comments here that give an overall description of what your
## functions do

## In here, we're creating a function that returns a list containing
## getter/setter for two things: the matrix itself, and its inverse.

makeCacheMatrix <- function(my.matrix = matrix()) {
  my.matrix.inverse <- NULL
  set <- function(the.matrix) {
    my.matrix <<- the.matrix
    my.matrix.inverse <<- NULL
  }
  get <- function() {
    my.matrix
  }
  setMatrixInverse <- function(the.matrix.inverse) {
    my.matrix.inverse <<- the.matrix.inverse
  }
  getMatrixInverse <- function() {
    my.matrix.inverse
  }
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

## In this function we always check if the inverse is already calculated
## first. If yes, just return it right away. 

cacheSolve <- function(the.cache.matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  the.probably.cached.matrix.inverse <- the.cache.matrix$getMatrixInverse()
  if(!is.null(the.probably.cached.matrix.inverse)) {
    message("returning cached matrix inverse")
    return(the.probably.cached.matrix.inverse)
  }
  ## if not, that means we don't have the inverse
  ## yet, so calculate it.
  the.matrix.itself <- the.cache.matrix$get()
  the.matrix.inverse <- solve(the.matrix.itself, ...)
  ## cache it now
  the.cache.matrix$setMatrixInverse(the.matrix.inverse)
  the.matrix.inverse
}
