## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) { ## set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x ## get the value of the matrix
  setinverse <- function(inverse) m <<- inverse ## set the value of the inverse matrix
  getinverse <- function() m ## get the value of the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## First checks to see if the inverse matrix has already been calculated.
  if(!is.null(m)) {
	## If so, it gets the inverse matrix from the cache and skips the computation.
	message("getting cached data")
	return(m)
  }
  ## Otherwise, it calculates the inverse matrix of the matrix and sets the 
  ## value of the mean in the cache via the setmean function.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
