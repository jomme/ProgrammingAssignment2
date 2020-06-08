## Put comments here that give an overall description of what your
## functions do

## This function constructs special 'matrix' object that is capable
## of caching its inverse.
## $set:        set the contents of the matrix and reset its inverse
## $get:        get the contents of the matrix
## $setinverse: set the contents of the inverse matrix (=cache)
## $getinverse: get the contents of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the special 'matrix' object and checks
## if a cached inverse exists. If not, it will calculate it
## and store it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
