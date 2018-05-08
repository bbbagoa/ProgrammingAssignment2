## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed.
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the 
# cache via setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data.")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data)
  x$setinverse(x_inv)
  x_inv
}
