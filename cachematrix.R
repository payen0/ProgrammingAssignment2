# These functions compute (if it doesn't exist yet) and store 
# the inverse of a matrix x in the cache. When computing the 
# inverse of x through cacheSolve, if it already exists, 
# the cached copy is called instead.

# this function returns a list X of methods to get the data x,
# set the inverse (when cacheSolve computes it) of x to X, and
# get the inverse, if it exists. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(get = get, setinv = setinv, getinv = getinv)
}

# this function checks if the inverse of x exists; if not,
# it is computed and stored in cache with x$setinv(inv) method;
# if the inverse exists, it is retrieved from cache and returned.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  inv
}