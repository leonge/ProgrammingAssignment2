## This is Assigment 2 from R Programming course from coursera.

## In this assigment, a pair of functions are writtent that allow to
## cache the inverse of a matrix

## The following function `makeCacheMatrix` creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get the matrix
  get <- function() x
  # Set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  # Get the inverse of the matrix
  getinverse <- function() m
  # Insert in a list.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get the state of the matrix inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Get the matrix
  data <- x$get()
  # Compute the inverse
  m <- solve(data, ...)
  # Cache the result
  x$setinverse(m)
  # Return the inverse
  m
}
