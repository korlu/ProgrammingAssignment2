## Caching the Inverse of a Matrix.
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. 
## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_x <<- inverse
  getInverse <- function() inv_x
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInverse()
  if (!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  mat <- x$get()
  inv_x <- solve(mat, ...)
  x$setInverse(inv_x)
  inv_x
}
