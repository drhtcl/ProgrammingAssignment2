## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Below is a pair of functions 
## that cache the inverse of a matrix.

## makeCacheMatrix is a function that will returns a special matrix
## which can be accessed through cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseIn) inverse <<- inverseIn
  getInverse <- function() inverse
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## This function will return a matrix that is the inverse of x.
## x is the special matrix returned by makeCacheMatrix().
cacheSolve <- function(x, ...) {
  ## try to get the inverse of x from cache.
  inverse <- x$getInverse()
  if (!is.null(inverse))
  {
    ## return data from cache!
    message("using cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  ## return the inverse
  inverse
}
