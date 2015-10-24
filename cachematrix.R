## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize cache value is null
  cache <- NULL
  
  ## creates the "matrix" object
  set <- function(y){
    x <<- y
    cache <<- NULL
  }
  
  ## get the value of the matrix
  get<- function() x
  
  ## set the invert matrix in cache
  setinverse <- function(inverse) cache <<- inverse
  
  ## get the invert matrix
  getinverse <- function () cache
  
  ## return the values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  
  ## get the inverse of the matrix created in makeCacheMatrix
  cache <- x$getinverse()
  
  ## first checks to see if the inversed matrix has already been exist. 
  ## If so, it gets the matrix from the cache and skips the computation.
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## For this assignment, assume that the matrix supplied is always invertible
  ## Computing the inverse matrix
  inverse <- solve(data, ...)
  
  ## set inverted matrix in cache
  x$setinverse(cache)
  
  ## retrieve the inverse from the cache
  cache
}
