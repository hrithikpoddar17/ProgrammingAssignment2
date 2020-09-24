##  Below are two functions that are used to create a special object that stores an invertible square matrix and cache's its inverse.


## makeCacheMatrix function creates a special object where the square matrix and its inverse is stored.

makeCacheMatrix <- function(x = matrix()) {
                          i <- NULL
                          set <- function(y) {
                                    x <<- y
                                    i <<- NULL
                          }
                          get <- function() x
                          setinv <- function(inv) i <<- inv
                          getinv <- function() i
                          list(set = set, get = get,
                          setinv = setinv,
                          getinv = getinv)
}


## cacheSolve function store and returns the inverse of the matrix.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
                      i <- x$getinv()
                      if(!is.null(i)) {
                              message("getting cached data")
                              return(i)
                      }
                      data <- x$get()
                      i <- solve(data, ...)
                      x$setinv(i)
                      i
}
