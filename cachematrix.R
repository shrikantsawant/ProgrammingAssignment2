## cachematrix.R
## Used to create a cache matrix object which can be used to solve inverse of a
## matrix, but will compute the inverse only once and cache it for subsequent use.
##
## Creates 2 functions makeCacheMatrix and cacheSolve 

## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)

## Sample: 
## M <- matrix(c(1,3,5,0,1,-1,-2,-2,9), nrow=3, ncol=3)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)

## Output:
##  > cacheSolve(cacheMatrix)
##  Getting from Cached data
##  [,1]       [,2]        [,3]
##  [1,]  0.3043478 0.08695652  0.08695652
##  [2,] -1.6086957 0.82608696 -0.17391304
##  [3,] -0.3478261 0.04347826  0.04347826
##  > 

## Assumptions :  The matrix supplied is always invertible.

## makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    cachedInverseMatrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) cachedInverseMatrix <<- inverse
  getInverse <- function() cachedInverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

## Fetch and Return inverse of a cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("Getting from Cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
