## Given an invertible square matrix, the functions below invert the matrix and
## store in the cache for the first call. Subsequent calls pull the store
## inverse from cache.

## The makeCaheMatrix function takes a given matrix (assumend invertible),
## computes its inverse and stores the result in the cache.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve(x)
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## The cacheSolve function checks if an inverse of a given matrix is already
## stored in the cache. If it is, pulls the inverse from cache, otherwise
## computes the inverse through makeCacheMatrix.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
