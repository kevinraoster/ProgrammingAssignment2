## This file contains a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(q, ...) {
        ## Return a matrix that is the inverse of 'q'
  m <- q$getsolve()
  if(!is.null(m)) {
    if (q==x){
    message("getting cached data")
    return(m)
    }
  }
  data <- q$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  }
