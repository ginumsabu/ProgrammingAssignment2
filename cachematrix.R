## Matrix inversion is a costly computation and these function caches the 
## inverse of a matrix rather than compute it repeatedly.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix1 = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix1 <<- y
    inverse <<- NULL
  }
  get <- function() matrix1
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(matrixobject, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- matrixobject$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- matrixobject$get()
  m <- solve(data)
  matrixobject$setinverse(m)
  m
}
