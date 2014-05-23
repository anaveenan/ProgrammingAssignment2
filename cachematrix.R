## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # inv will store the cached inverse matrix
  m <- NULL
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # getter for the matrix
  get <- function() x
  # Setter for the inverse
  setinverse <- function(inverse) m <<- inverse
  # getter for the inverse
  getinverse <- function() m
  # Return the matrix with our newly defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    
    return(m)
  }
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  m <- solve(data, ...)
  # Cache the inverse
  x$setinverse(m)
  # Return it
  m
}
