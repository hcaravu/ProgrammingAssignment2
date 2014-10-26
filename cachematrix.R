## This file contains two functions.
## makeCacheMatrix creates a special "matrix" object that can
## cache its inverse. cacheSolve computes the inverse of that matrix.
## If the inverse has already been calculated and the matrix
## hasn't changed, cacheSolve retrieves the inverse from the cache.

## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'X', either computing a new one
## or returning an existing cached one.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}