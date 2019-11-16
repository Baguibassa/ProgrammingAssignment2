# The cachematrix.R file contains two function, makeCacheMatrix() and cacheSolve().
# The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse.
# The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() 
# in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() 
# objects environment.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL # initialization of s
  # reset value with a new matrix
  set <- function(y) {
    x <<- y # assign the value of y to the x object in the parent environment
    s <<- NULL # assign the value of NULL to the s object in the parent environment
  }
  # get the value of matrix
  get <- function() {
    x
  } 
  setsolve <- function(solve) s <<- solve # set the value of the solve
  getsolve <- function() s # get the value of the solve
  # gives the name 'set', 'get', 'setsolve' and 'getsolve' respectively to the 
  #set(), get(), setsolve() and getsolve() function defined above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getsolve() # attempts to retrieve a solve from the object passed in as the argument
  if (!is.null(s)) { # checks to see whether s is NULL
    message("getting cached data")
    return(s) # if the s value is not equal to NULL, return a valid cached solve value to the parent environment
  }
  # If the result of !is.null(s) is FALSE
  data <- x$get() # cacheSolve() gets the matrix from the input object
  s <- solve(data, ...) # calculates a solve
  x$setsolve(s) # uses the setsolve() function on the input object to set the solve in the input object
  s # returns the value of the solve to the parent environment by printing the solve object
}
