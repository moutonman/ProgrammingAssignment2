## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() {
    return(x)
    }
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("from cached data")
  } else {
    m <- x$get()
    xinv <- solve(m, ...)
    x$setinv(xinv)
  }
  return(xinv)
}

