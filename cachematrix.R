## A pair of functions for calculating and caching the inverse of matrices.

## The makeCacheMatrix function creates a special object, or "matrix" that can cache the inverse of an input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function looks for a cached version of the input matrix -- if it can find one, retrieves and returns it. Otherwise, it calculates, caches, and returns the inverse of the special matrix returned by cacheMatrix above

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}