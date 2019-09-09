## The main goal of this script is to create a pair of functions that cache a 
## matrix then solve for the inverse of the matrix

## The first function "makeCacheMatrix" creates a special matrix object that can 
## cache its inverse (applies for a square matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(solve) m <<- solve
  getInverse <- function()m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function "cacheSolve"computes the inverse of the special matrix
## returned by makeCacheMatrix. If the inverse has already been calculatedm then this
## this function retreives the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
