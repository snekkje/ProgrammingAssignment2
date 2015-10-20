## We are assuming here that a matrix "X" is an invertable matrix that if it were large
## would be computationally difficult to invert. The idea is that after calculating the
## matrix inverse "in(X)" it would store this in memory so that if it were called again it
## would not have to be computed again.


## This function returns a list comprising functions:

## "set": sets a prevously assigned "makeCacheMatrix" matrix "X"
## "get": returns the matrix "X"
## "setInverse": is (and should be only!) called from cacheSolve to solve for the inverse of "X"
## "getInverse": returns the matrix inverse "in(X)"

makeCacheMatrix <- function(X = matrix()) {
  matrixInverse <- NULL
  set <- function(Y) {
    X <<- Y
    matrixInverse <<- NULL
  }
  get <- function() X
  setInverse <- function(inverse) matrixInverse <<- inverse
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates and returns the inverse of matrix "X" if the inverse is
## not already stored in memory from a previous calculation. Otherwise it returns the
## cached inverse

cacheSolve <- function(X, ...) {
  matrixInverse <- X$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- X$get()
  matrixInverse <- solve(data, ...)
  X$setInverse(matrixInverse)
  matrixInverse
}
