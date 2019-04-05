## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function makes a matrix object that can cache the inverse of the object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function () inv
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix returned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}