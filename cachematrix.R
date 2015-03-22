## These functions calculate the inverse of a matrix and cache the value so that subsequent calls do not need 
## to recalculate the inverse.

## Function makeCacheMatrix takes parameter x of type matrix.  It returns a list of four functions that will
## respectively set the matrix, get the matrix, set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv) {
    i <<- inv
  }
  getinv <- function() {
    i
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Function cacheSolve takes a list of functions as input.  Making use of these functions, it first
## tries to return the inverse of matrix x from cache.  If the inverse of matrix x is not already cached,
## it calculates it by calling the solve function, then caches the inverse of x before returning its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting inverse from cache")
    return (i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
  
}
