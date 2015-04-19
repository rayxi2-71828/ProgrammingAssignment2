## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(my.matrix = matrix()) {
  # Create a special kind of matrix, one that can cache its own inverse
  #
  # Args:
  #   my.matrix: The "raw" matrix whose inverse we want to cache
  #
  # Returns:
  #   A list containing four functions: getter/setter for the matrix, 
  #   and the getter/setter of its inverse.
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
  # Given the special cache matrix created using the makeCacheMatrix
  # function above, either return the cached inverse if it's been 
  # calculated before, or calculate it and store it in the cache.
  #
  # Args:
  #   the.cache.matrix: The special cache matrix whose inverse we want to get
  #   ...: further arguments to pass to the solve method.
  #
  # Returns:
  #   The inverse of the raw matrix contained within the cache matrix.
  
  # rudimentary check 
  if(class(the.cache.matrix) != "list") {
    stop("Please use makeCacheMatrix to wrap your matrix first")
  }
  
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
