## This file contains a pair of functions (makeCacheMatrix and cacheSolve) that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ##This function sets user input  as the value of the matrix and initializes a variable 'm' (which represents cache)
    x <<- y
    m <<- NULL
  }
  get <- function() x ## This function retrives the value of the matrix
  setsolve <- function(p) m <<- p ## This function assigns the input to 'm' (which represents the cache)
  getsolve <- function() m ##This function retrieves the value of m(which represents the cache)
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(q) {
        ## Return a matrix that is the inverse of 'x'
  m <- q$getsolve() ## Get the value from cache
  if(!is.null(m)) {
    ## Return inverse from cache if it exists
    message("getting cached data")
    return(m)
  }
  ## compute the inverse if cache is empty or if the matrix has changed
  data <- q$get()
  m <- solve(data)
  q$setsolve(m)
  m
  }
