# The cachematrix.R file contains two function, makeCacheMatrix() and cacheSolve().
# The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse.
# The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() 
# in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() 
# objects environment.

# This function creates a special "matrix" object that can cache its inverse.
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
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if (!is.null(s)) { 
    message("getting cached data")
    return(s)
  }
  # If the result of !is.null(s) is FALSE
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
