## This R script is created with two function to cache and calculate
##     inverse of a squate matrix.
## Assumption: supplied matrix is always invertible.
## Programming Assignment# 2
## Date: 12/27/2015

## Following function makes a cache list with matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invMatrix) m <<- invMatrix
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## This function calculates matrix inverse if cache is empty
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
