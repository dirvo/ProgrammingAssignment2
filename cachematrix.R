## R Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## a variable to cache the inverse of the matrix
  inverse <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the cached version of the inverse matrix
  setinverse <- function(inv) inverse <<- inv
  
  ## get the cached inverse matrix
  getinverse <- function() inverse
  
  ## return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## get the cached inverse
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## no cached data found, get the matrix
  data <- x$get()
  
  ## calculate the inverse matrix using the standard function solve()
  inverse <- solve(data, ...)
  
  ## cache the result
  x$setinverse(inverse)
  
  ## return the inverse
  inverse
}

