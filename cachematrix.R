################################################################################
## Matrix inversion is usually a costly computation so there is a perfromance 
## benefit in caching the result, rather than computing it repeatedly.
##
## makeCacheMatrix and cacheSolve below are functions that implement caching of 
## the inverse of an unchanged matrix.
##
## NOTE: the following implementation assumes the matrix supplied is always an 
##       invertible square matrix to allow for the use of the "solve(X)" 
##       function
##
## Example usage is as follows:
##
## source('./cachematrix.R', echo=FALSE)
## mat <- matrix(c(4,3,6,3), nrow=2,ncol=2) 
## cacheMatrix <- makeCacheMatrix(mat)
## cacheSolve(cacheMatrix)
## cacheSolve(cacheMatrix)
##
## this results in the inverted matrix being printed twice - but with 
## "getting cached data" printed before the second time to indicate the cached 
## inverse was used e.g.:
##
##> cacheSolve(cacheMatrix)
##[,1]       [,2]
##[1,] -0.5  1.0000000
##[2,]  0.5 -0.6666667
##
##> cacheSolve(cacheMatrix)
##getting cached data
##[,1]       [,2]
##[1,] -0.5  1.0000000
##[2,]  0.5 -0.6666667
#
################################################################################

## Creates a special "matrix" object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
##
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
