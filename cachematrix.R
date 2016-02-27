## The combination of these functions creates a special matrix object that can cache its inverse
## The first function, makeCacheMatrix, creates this special matrix and returns a list 
## that is used as an input into the second function, cacheSolve(), which returns a matrix that
## is its inverse.

## makeCacheMatrix creates a special matrix object that is capable of caching its inverse. This
## can speed up the processing time.

makeCacheMatrix <- function(x = matrix()) {
  ##  This list is used as the input to the cacheSolve() function
  
  inv = NULL
  ## Set the matrix that is cached (using <<- )
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Get the matrix
  get = function() x
  ## Set the inverse
  setinv = function(inverse) inv <<- inverse 
  ## Get the inverse
  getinv = function() inv
  ## Return a list for setting, getting, setting inverse, and getting inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve function takes as an input a special matrix (returned by makeChacheMatrix) 
## and returns a matrix that is the inverse of x.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  inv = x$getinv()
  
  # check to see if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the calculation
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculate the inverse 
  mdata = x$get()
  inv = solve(mdata, ...)
  
  # set the value of the inverse in the cache 
  x$setinv(inv)
  
  return(inv)
}
