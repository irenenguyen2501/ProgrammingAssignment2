## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## This function returns a list of functions that:
##  - set the value of a matrix
##  - get the value of the matrix
##  - set the value of the inverse
##  - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL   # 'j' will store the cached inverse, initially NULL
  
  # Setter function: assign a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y     # save the new matrix into the parent environment
    j <<- NULL  # reset the cached inverse because the matrix has changed
  }
  
  # Getter function: return the matrix
  get <- function() x
  
  # Setter for inverse: cache the inverse matrix
  setInverse <- function(inverse) j <<- inverse
  
  # Getter for inverse: return the cached inverse
  getInverse <- function() j 
  
  # Return a list of the above four functions so they can be accessed
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve retrieves the inverse from the cache instead of recomputing.
cacheSolve <- function(x, ...) {
  ## Try to get the cached inverse
  j <- x$getInverse()
  
  # If cached inverse exists, return it directly
  if(!is.null(j)) {
    message("getting cached data")  # notify that cached value is used
    return(j)
  }
  
  # Otherwise, compute the inverse of the matrix
  mat <- x$get()
  j <- solve(mat, ...)   # solve() computes the inverse of a matrix
  
  # Cache the newly computed inverse
  x$setInverse(j)
  
  # Return the inverse
  j
}
