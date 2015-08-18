## Compute inverse of a matrix and cache it
## Create a 'special' matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setm <- function(y) {
    x<<-y
    inv <<- NULL
  }
  getm <- function() x
  setinverse <-function(y) {
    inv <<- y
  }
  getinverse <-function() {
    inv
  }
  list(setm=setm, getm=getm, setinverse=setinverse, getinverse=getinverse)
}

## function computing the inverese of a 'special' matrix x returned by the function "makeCacheMatrix". The inverse is computed only if it hasn't been calculated before.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$getm()
  m<-solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

