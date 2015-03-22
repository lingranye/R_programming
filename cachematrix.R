

## This function is used to make cache matrix. It contains four functions.
## First is to set matrix. Second is to get this matrix.
## Third is to calculate the inverse of this matrix. Fourth is to get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve
       getsolve = getsolve)
}


## First this function is to test whether the x has its inverse. 
## If its inverse exists, return the inverse. Otherwise, calcuate the inverse for X.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
