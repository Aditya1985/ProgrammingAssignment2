## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(chol2inv) inv_matrix <<- chol2inv
  getinverse <- function() inv_matrix
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- chol2inv(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
}


## x = matrix(1:4, 2, 2)
## x
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## m = makeCacheMatrix(x)
## m$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(m)
##        [,1]    [,2]
## [1,]  1.5625 -0.1875
## [2,] -0.1875  0.0625
## cacheSolve(m)
## getting cached data
##        [,1]    [,2]
## [1,]  1.5625 -0.1875
## [2,] -0.1875  0.0625