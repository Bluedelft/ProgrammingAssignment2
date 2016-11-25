## This function allows to invert matrix and cache it in order to retrieve the same matrix
## if this didn't change from the cache. This will save a lot of computational time.

## In the following first function 
## set a matrix
## get a matrix
## set the inverse matrix
## get the inverse matrix 
## This will be used for the next function (cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The next function takes in input the output of makeCacheMatrix


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

## if the inverse matrix is already calculated it gives me a message and take the
## inverse matrix from the cache

    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
## otherwise calculates the inverse matrix
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)
  
}