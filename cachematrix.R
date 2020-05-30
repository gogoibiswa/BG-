## CACHING THE INVERSE OF A MATRIX
## Matrix inversion is usually costly computation.
## There are benefits of caching the inverse of matrix as showed below.
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
      x <<- y
      j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
  }

## The following function computes the inverse of the matrix created above.
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
